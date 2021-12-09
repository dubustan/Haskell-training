{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Monad (void)
import Control.Lens
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Data.Text (pack, Text)
import qualified Ledger
import qualified Ledger.Ada as Ada
import qualified Ledger.Constraints as Constraints
import qualified Ledger.Scripts as Scripts
import qualified Ledger.Typed.Scripts as TScripts
import qualified Ledger.Value as Value
import qualified Plutus.Contract as Contract
import qualified Plutus.Contract.Wallet as Wallet
import qualified Plutus.Trace.Emulator as Trace
import qualified Plutus.Contracts.Currency as Currency
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AssocMap
import PlutusTx.Prelude (traceIfFalse)
import Plutus.Trace.Emulator
import Wallet.Emulator (knownWallet)
import Prelude
import Text.Printf (printf)

data CDPDatum = ManagerDatum {getList :: [Ledger.PubKeyHash]} | UserDatum {getPKH :: Ledger.PubKeyHash, getADA :: Integer, getMinted :: Integer} deriving Show

data CDPAction = CDPInit | CDPOpen | CDPDeposit | CDPWithdraw | CDPMint | CDPBurn
PlutusTx.unstableMakeIsData ''CDPDatum
PlutusTx.unstableMakeIsData ''CDPAction
PlutusTx.makeLift ''CDPAction

data CDP
instance TScripts.ValidatorTypes CDP where
    type DatumType CDP = CDPDatum
    type RedeemerType CDP = CDPAction

{-# INLINEABLE mkValidator #-}
mkValidator :: CDPDatum -> CDPAction -> Ledger.ScriptContext -> Bool
mkValidator datum action scriptcontext = case action of
    CDPInit -> traceIfFalse "No input needed" ((length $ Ledger.txInfoInputs $ Ledger.scriptContextTxInfo scriptcontext) == 0)
    CDPOpen -> True
    CDPDeposit -> True
    CDPWithdraw -> True
    CDPMint -> True
    CDPBurn -> True

cdpInstance :: TScripts.TypedValidator CDP
cdpInstance = 
    TScripts.mkTypedValidator @CDP
        $$(PlutusTx.compile [||mkValidator||])
        $$(PlutusTx.compile [||wrap||])
  where 
    wrap = TScripts.wrapValidator @CDPDatum @CDPAction

cdpValidator :: TScripts.Validator
cdpValidator = TScripts.validatorScript cdpInstance

cdpAddress :: Ledger.Address
cdpAddress = Ledger.scriptAddress cdpValidator

{-# INLINEABLE mkPolicy #-}
mkPolicy :: Ledger.TxOutRef -> Ledger.ScriptContext -> Bool
mkPolicy _ _ = True

mintingPolicy :: TScripts.MintingPolicy
mintingPolicy =
    Ledger.mkMintingPolicyScript
        $$(PlutusTx.compile [||TScripts.wrapMintingPolicy mkPolicy||])

mintingPolicyHash :: Scripts.MintingPolicyHash
mintingPolicyHash = Scripts.mintingPolicyHash mintingPolicy

cdpCurSymbol :: Value.CurrencySymbol
cdpCurSymbol = Value.mpsSymbol mintingPolicyHash

cdpTokenName :: Value.TokenName
cdpTokenName = "iTSLA"

type CDPSchema = Contract.Endpoint "Open" () Contract..\/ Contract.Endpoint "Deposit" () Contract..\/ Contract.Endpoint "Withdraw" () Contract..\/ Contract.Endpoint "Mint" () Contract..\/ Contract.Endpoint "Burn" ()

getDatum :: PlutusTx.FromData b => Ledger.ChainIndexTxOut -> Maybe b
getDatum o = preview Ledger.ciTxOutDatum o >>= rightToMaybe >>= (PlutusTx.fromBuiltinData . Ledger.getDatum)

getValue :: Ledger.ChainIndexTxOut -> Ledger.Value
getValue = view Ledger.ciTxOutValue

rightToMaybe :: Either a b -> Maybe b
rightToMaybe x = case x of
    Left a -> Nothing
    Right b -> Just b

mkCurrency :: Ledger.TxOutRef -> [(Value.TokenName, Integer)] -> Currency.OneShotCurrency
mkCurrency (Ledger.TxOutRef h i) amts =
    Currency.OneShotCurrency
        { Currency.curRefTransactionOutput = (h, i)
        , Currency.curAmounts              = AssocMap.fromList amts
        }    

init :: Contract.Contract w s Currency.CurrencyError Currency.OneShotCurrency
init = do
    ownPubKey <- Contract.ownPubKeyHash
    txOutRef <- Wallet.getUnspentOutput
    utxos <- Contract.utxosAt (Ledger.pubKeyHashAddress ownPubKey)
    let theCurrency = mkCurrency txOutRef [("authToken", 1)]
        curVali     = Currency.curPolicy theCurrency
        lookups     = Constraints.mintingPolicy curVali
                    <> Constraints.unspentOutputs utxos
        tx          = Constraints.mustSpendPubKeyOutput txOutRef
                    <> Constraints.mustMintValue (Currency.mintedValue theCurrency)
                    <> Constraints.mustPayToTheScript (ManagerDatum []) (Currency.mintedValue theCurrency) 
    void $ Contract.submitTxConstraintsWith lookups tx >>= Contract.awaitTxConfirmed . Ledger.getCardanoTxId
    pure theCurrency        

open :: Contract.Contract w s Contract.ContractError ()
open = do
    managers <- M.filter isManagerOutput <$> Contract.utxosAt cdpAddress
    myKey <- Contract.ownPubKeyHash
    if (length $ M.toList managers) == 0 
        then Contract.throwError "Havent initiated"
        else 
            do
                let (oref, o) = head $ M.toList managers
                    managerDatum = (fromJust $ getDatum @CDPDatum o)
                    cdpOpenedList = (getList managerDatum)
                if myKey `elem` cdpOpenedList
                    then
                        Contract.throwError "Can't open CDP"
                    else do
                        let lookups = Constraints.typedValidatorLookups cdpInstance
                            tx = Constraints.mustSpendScriptOutput oref (Scripts.Redeemer $ PlutusTx.toBuiltinData CDPOpen)
                                <> Constraints.mustPayToTheScript (UserDatum myKey 0 0) (Ada.lovelaceValueOf 0)
                                <> Constraints.mustPayToTheScript (ManagerDatum $ myKey : cdpOpenedList) (getValue o) 
                        void $ Contract.submitTxConstraintsWith lookups tx >>= Contract.awaitTxConfirmed . Ledger.getCardanoTxId
    where
        isManagerOutput :: Ledger.ChainIndexTxOut -> Bool
        isManagerOutput o = undefined

deposit :: Integer -> Contract.Contract w s Contract.ContractError ()
deposit amount = do
    myKey <- Contract.ownPubKeyHash
    cdputxo <- M.filter (isCDPOutput myKey) <$> Contract.utxosAt cdpAddress
    if amount < 0
        then Contract.throwError "The amount must be positive"
        else do
            if (length $ M.toList cdputxo) == 0
                then Contract.throwError "Havent opened the CDP"
                else do
                    let (oref, o) = head $ M.toList cdputxo
                        userDatum = fromJust $ getDatum @CDPDatum o
                        lookups = Constraints.typedValidatorLookups cdpInstance 
                                <> Constraints.unspentOutputs (M.fromList [(oref, o)])
                        constraints = Constraints.mustSpendScriptOutput oref (Scripts.Redeemer (PlutusTx.toBuiltinData CDPDeposit)) 
                                    <> Constraints.mustPayToTheScript (UserDatum myKey ((getADA userDatum) + amount) (getMinted userDatum)) (getValue o <> Ada.lovelaceValueOf amount)
                    Contract.submitTxConstraintsWith lookups constraints >>= Contract.awaitTxConfirmed . Ledger.getCardanoTxId
    where
        isCDPOutput :: Ledger.PubKeyHash -> Ledger.ChainIndexTxOut -> Bool
        isCDPOutput myk o = case (fromJust $ getDatum @CDPDatum o) of
            ManagerDatum md -> False
            UserDatum cd _ _ -> cd == myk

withdraw :: Integer -> Contract.Contract w s Contract.ContractError () 
withdraw amount = do
    myKey <- Contract.ownPubKeyHash
    cdputxo <- M.filter (isCDPOutput myKey) <$> Contract.utxosAt cdpAddress
    if amount < 0
        then Contract.throwError "The amount must be positive"
        else do
            if (length $ M.toList cdputxo) == 0
                then Contract.throwError "Havent opened the CDP"
                else do 
                    let (oref, o) = head $ M.toList cdputxo
                        userDatum = fromJust $ getDatum @CDPDatum o
                    if amount > (getADA userDatum)
                        then Contract.throwError "Not enough ADA to withdraw"
                        else do
                            if 130 * ((getADA userDatum) - amount) < 70967 * (getMinted userDatum) * 150 * 100000000
                                then Contract.throwError "The minimal Collateral Ratio should be maintained" 
                                else do
                                    let lookups = Constraints.typedValidatorLookups cdpInstance
                                                <> Constraints.unspentOutputs (M.fromList [(oref, o)])
                                        constraints = Constraints.mustSpendScriptOutput oref (Scripts.Redeemer (PlutusTx.toBuiltinData CDPWithdraw)) 
                                                    <> Constraints.mustPayToTheScript (UserDatum myKey ((getADA userDatum) - amount) (getMinted userDatum)) (getValue o <> Ada.lovelaceValueOf (-amount))
                                    Contract.submitTxConstraintsWith lookups constraints >>= Contract.awaitTxConfirmed . Ledger.getCardanoTxId
    where
        isCDPOutput :: Ledger.PubKeyHash -> Ledger.ChainIndexTxOut -> Bool
        isCDPOutput myk o = case (fromJust $ getDatum @CDPDatum o) of
            ManagerDatum md -> False
            UserDatum cd _ _ -> cd == myk

mint :: Integer -> Contract.Contract w s Contract.ContractError ()
mint amount = do
    myKey <- Contract.ownPubKeyHash
    cdputxo <- M.filter (isCDPOutput myKey) <$> Contract.utxosAt cdpAddress
    if amount < 0
        then Contract.throwError "The amount must be positive"
        else do
            if (length $ M.toList cdputxo) == 0
                then Contract.throwError "Havet opened the CDP"
                else do
                    let (oref, o) = head $ M.toList cdputxo
                        userDatum = fromJust $ getDatum @CDPDatum o
                    if 130 * (getADA userDatum) < 70967 * ((getMinted userDatum) + amount) * 150 * 100000000
                        then Contract.throwError "The minimal Collateral Ratio should be maintained"
                        else do
                            let val = Value.assetClassValue (Value.assetClass cdpCurSymbol cdpTokenName) amount
                                lookups = Constraints.typedValidatorLookups cdpInstance
                                        <> Constraints.unspentOutputs (M.fromList [(oref, o)])
                                        <> Constraints.mintingPolicy mintingPolicy
                                constraints = Constraints.mustSpendScriptOutput oref (Scripts.Redeemer (PlutusTx.toBuiltinData CDPMint))
                                            <> Constraints.mustMintValue val
                                            <> Constraints.mustPayToTheScript (UserDatum myKey (getADA userDatum) ((getMinted userDatum) + amount)) (getValue o)
                            Contract.submitTxConstraintsWith lookups constraints >>= Contract.awaitTxConfirmed . Ledger.getCardanoTxId
    where
        isCDPOutput :: Ledger.PubKeyHash -> Ledger.ChainIndexTxOut -> Bool
        isCDPOutput myk o = case (fromJust $ getDatum @CDPDatum o) of
            ManagerDatum md -> False
            UserDatum cd _ _ -> cd == myk

burn :: Integer -> Contract.Contract w s Contract.ContractError ()
burn amount = do
    myKey <- Contract.ownPubKeyHash
    cdputxo <- M.filter (isCDPOutput myKey) <$> Contract.utxosAt cdpAddress
    if amount < 0
        then Contract.throwError "The amount must be positive"
        else do
            if (length $ M.toList cdputxo) == 0
                then Contract.throwError "Havet opened the CDP"
                else do
                    let (oref, o) = head $ M.toList cdputxo
                        userDatum = fromJust $ getDatum @CDPDatum o
                    if amount > getMinted userDatum
                        then Contract.throwError "The minimal Collateral Ratio should be maintained"
                        else do
                            let val = Value.assetClassValue (Value.assetClass cdpCurSymbol cdpTokenName) (-amount)
                                lookups = Constraints.typedValidatorLookups cdpInstance
                                        <> Constraints.unspentOutputs (M.fromList [(oref, o)])
                                        <> Constraints.mintingPolicy mintingPolicy
                                constraints = Constraints.mustSpendScriptOutput oref (Scripts.Redeemer (PlutusTx.toBuiltinData CDPMint))
                                             <> Constraints.mustMintValue val
                                            <> Constraints.mustPayToTheScript (UserDatum myKey (getADA userDatum) ((getMinted userDatum) - amount)) (getValue o)
                            Contract.submitTxConstraintsWith lookups constraints >>= Contract.awaitTxConfirmed . Ledger.getCardanoTxId
    where
        isCDPOutput :: Ledger.PubKeyHash -> Ledger.ChainIndexTxOut -> Bool
        isCDPOutput myk o = case (fromJust $ getDatum @CDPDatum o) of
            ManagerDatum md -> False
            UserDatum cd _ _ -> cd == myk

main :: IO ()
main = do
    print "Hello"
