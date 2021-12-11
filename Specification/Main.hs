{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

module Main where

import Control.Monad (void)
import Control.Lens
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Data.Text (pack, Text)
import Data.Aeson (FromJSON, ToJSON)
import Data.Monoid (Last (Last))
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
import GHC.Generics 

data CDPDatum = ManagerDatum {getList :: [Ledger.PubKeyHash]} | UserDatum {getPKH :: Ledger.PubKeyHash, getADA :: Integer, getMinted :: Integer} deriving Show

data UserParams = UserParams {upnft :: Value.Value, upamount :: Integer} deriving (FromJSON, ToJSON, Monoid, Generic, Semigroup)
data ValidatorParams = ValidatorParams {vpnft :: Value.Value}

data CDPAction = CDPInit | CDPOpen | CDPDeposit | CDPWithdraw | CDPMint | CDPBurn
PlutusTx.unstableMakeIsData ''CDPDatum
PlutusTx.unstableMakeIsData ''CDPAction
PlutusTx.unstableMakeIsData ''ValidatorParams
PlutusTx.unstableMakeIsData ''UserParams
PlutusTx.makeLift ''CDPAction
PlutusTx.makeLift ''ValidatorParams

data CDP
instance TScripts.ValidatorTypes CDP where
    type DatumType CDP = CDPDatum
    type RedeemerType CDP = CDPAction

{-# INLINEABLE mkValidator #-}
mkValidator :: ValidatorParams -> CDPDatum -> CDPAction -> Ledger.ScriptContext -> Bool
mkValidator params datum action scriptcontext = case action of
    CDPInit -> traceIfFalse "No input needed" (null $ Ledger.txInfoInputs $ Ledger.scriptContextTxInfo scriptcontext)
    CDPOpen -> True
    CDPDeposit -> True
    CDPWithdraw -> True
    CDPMint -> True
    CDPBurn -> True

cdpInstance :: ValidatorParams -> TScripts.TypedValidator CDP
cdpInstance params = 
    TScripts.mkTypedValidator @CDP
        ($$(PlutusTx.compile [||mkValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode params) 
        $$(PlutusTx.compile [||wrap||])
  where 
    wrap = TScripts.wrapValidator @CDPDatum @CDPAction

cdpValidator :: ValidatorParams -> TScripts.Validator
cdpValidator = TScripts.validatorScript . cdpInstance

cdpAddress :: ValidatorParams -> Ledger.Address
cdpAddress = Ledger.scriptAddress . cdpValidator

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

getDatum :: PlutusTx.FromData b => Ledger.ChainIndexTxOut -> Maybe b
getDatum o = preview Ledger.ciTxOutDatum o >>= rightToMaybe >>= (PlutusTx.fromBuiltinData . Ledger.getDatum)

getValue :: Ledger.ChainIndexTxOut -> Ledger.Value
getValue = view Ledger.ciTxOutValue

rightToMaybe :: Either a b -> Maybe b
rightToMaybe x = case x of
    Left a -> Nothing
    Right b -> Just b

isEqual :: Value.Value -> Value.Value -> Bool
isEqual a b = (Value.flattenValue a) == (Value.flattenValue b)

mkCurrency :: Ledger.TxOutRef -> [(Value.TokenName, Integer)] -> Currency.OneShotCurrency
mkCurrency (Ledger.TxOutRef h i) amts =
    Currency.OneShotCurrency
        { Currency.curRefTransactionOutput = (h, i)
        , Currency.curAmounts              = AssocMap.fromList amts
        }

init :: Contract.Contract w s Currency.CurrencyError Value.Value
init = do
    ownPubKey <- Contract.ownPubKeyHash
    txOutRef <- Wallet.getUnspentOutput
    utxos <- Contract.utxosAt (Ledger.pubKeyHashAddress ownPubKey)
    let theCurrency = mkCurrency txOutRef [("authToken", 1)]
        curVali     = Currency.curPolicy theCurrency
        lookups     = Constraints.typedValidatorLookups (cdpInstance $ ValidatorParams $ Currency.mintedValue theCurrency)
                    <> Constraints.mintingPolicy curVali
                    <> Constraints.unspentOutputs utxos
        tx          = Constraints.mustSpendPubKeyOutput txOutRef
                    <> Constraints.mustMintValue (Currency.mintedValue theCurrency)
                    <> Constraints.mustPayToTheScript (ManagerDatum []) (Currency.mintedValue theCurrency) 
    void $ Contract.submitTxConstraintsWith lookups tx >>= Contract.awaitTxConfirmed . Ledger.getCardanoTxId
    return $ Currency.mintedValue theCurrency

len :: [a] -> Integer
len a = case a of
    [] -> 0
    x:xs -> 1 + len xs

open :: UserParams -> Contract.Contract w s Contract.ContractError ()
open UserParams{..} = do
    managers <- M.filter isManagerOutput <$> Contract.utxosAt (cdpAddress $ ValidatorParams $ upnft)
    myKey <- Contract.ownPubKeyHash
    if (length $ M.toList managers) == 0
        then Contract.throwError "Havent initiated"
        else
            do
                if ((len $ M.toList managers) /= 1)
                    then
                        Contract.throwError "There must be one manager"
                    else do
                        let (oref, o) = head $ M.toList managers
                            managerDatum = (fromJust $ getDatum @CDPDatum o)
                            cdpOpenedList = (getList managerDatum)
                        if myKey `elem` cdpOpenedList
                            then
                                Contract.throwError "Can't open CDP"
                            else do
                                let lookups = Constraints.typedValidatorLookups (cdpInstance $ ValidatorParams $ upnft)
                                    tx = Constraints.mustSpendScriptOutput oref (Scripts.Redeemer $ PlutusTx.toBuiltinData CDPOpen)
                                        <> Constraints.mustPayToTheScript (UserDatum myKey 0 0) (Ada.lovelaceValueOf 0)
                                        <> Constraints.mustPayToTheScript (ManagerDatum $ myKey : cdpOpenedList) (getValue o) 
                                void $ Contract.submitTxConstraintsWith lookups tx >>= Contract.awaitTxConfirmed . Ledger.getCardanoTxId
    where
        isManagerOutput :: Ledger.ChainIndexTxOut -> Bool
        isManagerOutput o = (getValue o) == upnft

deposit :: UserParams -> Contract.Contract w s Contract.ContractError ()
deposit UserParams{..} = do
    myKey <- Contract.ownPubKeyHash
    cdputxo <- M.filter (isCDPOutput myKey) <$> Contract.utxosAt (cdpAddress $ ValidatorParams $ upnft)
    if upamount < 0
        then Contract.throwError "The amount must be positive"
        else do
            if (length $ M.toList cdputxo) == 0
                then Contract.throwError "Havent opened the CDP"
                else do
                    let (oref, o) = head $ M.toList cdputxo
                        userDatum = fromJust $ getDatum @CDPDatum o
                        lookups = Constraints.typedValidatorLookups (cdpInstance $ ValidatorParams $ upnft) 
                                <> Constraints.unspentOutputs (M.fromList [(oref, o)])
                        constraints = Constraints.mustSpendScriptOutput oref (Scripts.Redeemer (PlutusTx.toBuiltinData CDPDeposit)) 
                                    <> Constraints.mustPayToTheScript (UserDatum myKey ((getADA userDatum) + upamount) (getMinted userDatum)) (getValue o <> Ada.lovelaceValueOf upamount)
                    Contract.submitTxConstraintsWith lookups constraints >>= Contract.awaitTxConfirmed . Ledger.getCardanoTxId
    where
        isCDPOutput :: Ledger.PubKeyHash -> Ledger.ChainIndexTxOut -> Bool
        isCDPOutput myk o = case (fromJust $ getDatum @CDPDatum o) of
            ManagerDatum md -> False
            UserDatum cd _ _ -> cd == myk

withdraw :: UserParams -> Contract.Contract w s Contract.ContractError () 
withdraw UserParams{..} = do
    myKey <- Contract.ownPubKeyHash
    cdputxo <- M.filter (isCDPOutput myKey) <$> Contract.utxosAt (cdpAddress $ ValidatorParams $ upnft)
    if upamount < 0
        then Contract.throwError "The amount must be positive"
        else do
            if (length $ M.toList cdputxo) == 0
                then Contract.throwError "Havent opened the CDP"
                else do 
                    let (oref, o) = head $ M.toList cdputxo
                        userDatum = fromJust $ getDatum @CDPDatum o
                    if upamount > (getADA userDatum)
                        then Contract.throwError "Not enough ADA to withdraw"
                        else do
                            if 130 * ((getADA userDatum) - upamount) < 70967 * (getMinted userDatum) * 150 * 100000000
                                then Contract.throwError "The minimal Collateral Ratio should be maintained" 
                                else do
                                    let lookups = Constraints.typedValidatorLookups (cdpInstance $ ValidatorParams $ upnft)
                                                <> Constraints.unspentOutputs (M.fromList [(oref, o)])
                                        constraints = Constraints.mustSpendScriptOutput oref (Scripts.Redeemer (PlutusTx.toBuiltinData CDPWithdraw)) 
                                                    <> Constraints.mustPayToTheScript (UserDatum myKey ((getADA userDatum) - upamount) (getMinted userDatum)) (getValue o <> Ada.lovelaceValueOf (-upamount))
                                    Contract.submitTxConstraintsWith lookups constraints >>= Contract.awaitTxConfirmed . Ledger.getCardanoTxId
    where
        isCDPOutput :: Ledger.PubKeyHash -> Ledger.ChainIndexTxOut -> Bool
        isCDPOutput myk o = case (fromJust $ getDatum @CDPDatum o) of
            ManagerDatum md -> False
            UserDatum cd _ _ -> cd == myk

mint :: UserParams -> Contract.Contract w s Contract.ContractError ()
mint UserParams{..} = do
    myKey <- Contract.ownPubKeyHash
    cdputxo <- M.filter (isCDPOutput myKey) <$> Contract.utxosAt (cdpAddress $ ValidatorParams $ upnft)
    if upamount < 0    
        then Contract.throwError "The amount must be positive"
        else do
            if (length $ M.toList cdputxo) == 0
                then Contract.throwError "Havet opened the CDP"
                else do
                    let (oref, o) = head $ M.toList cdputxo
                        userDatum = fromJust $ getDatum @CDPDatum o
                    if 130 * (getADA userDatum) < 70967 * ((getMinted userDatum) + upamount) * 150 * 100000000
                        then Contract.throwError "The minimal Collateral Ratio should be maintained"
                        else do
                            let val = Value.assetClassValue (Value.assetClass cdpCurSymbol cdpTokenName) upamount
                                lookups = Constraints.typedValidatorLookups (cdpInstance $ ValidatorParams $ upnft)
                                        <> Constraints.unspentOutputs (M.fromList [(oref, o)])
                                        <> Constraints.mintingPolicy mintingPolicy
                                constraints = Constraints.mustSpendScriptOutput oref (Scripts.Redeemer (PlutusTx.toBuiltinData CDPMint))
                                            <> Constraints.mustMintValue val
                                            <> Constraints.mustPayToTheScript (UserDatum myKey (getADA userDatum) ((getMinted userDatum) + upamount)) (getValue o)
                            Contract.submitTxConstraintsWith lookups constraints >>= Contract.awaitTxConfirmed . Ledger.getCardanoTxId
    where
        isCDPOutput :: Ledger.PubKeyHash -> Ledger.ChainIndexTxOut -> Bool
        isCDPOutput myk o = case (fromJust $ getDatum @CDPDatum o) of
            ManagerDatum md -> False
            UserDatum cd _ _ -> cd == myk

burn :: UserParams -> Contract.Contract w s Contract.ContractError ()
burn UserParams{..} = do
    myKey <- Contract.ownPubKeyHash
    cdputxo <- M.filter (isCDPOutput myKey) <$> Contract.utxosAt (cdpAddress $ ValidatorParams $ upnft)
    if upamount < 0
        then Contract.throwError "The amount must be positive"
        else do
            if (length $ M.toList cdputxo) == 0
                then Contract.throwError "Havet opened the CDP"
                else do
                    let (oref, o) = head $ M.toList cdputxo
                        userDatum = fromJust $ getDatum @CDPDatum o
                    if upamount > getMinted userDatum
                        then Contract.throwError "The minimal Collateral Ratio should be maintained"
                        else do
                            let val = Value.assetClassValue (Value.assetClass cdpCurSymbol cdpTokenName) (-upamount)
                                lookups = Constraints.typedValidatorLookups (cdpInstance $ ValidatorParams $ upnft)
                                        <> Constraints.unspentOutputs (M.fromList [(oref, o)])
                                        <> Constraints.mintingPolicy mintingPolicy
                                constraints = Constraints.mustSpendScriptOutput oref (Scripts.Redeemer (PlutusTx.toBuiltinData CDPMint))
                                             <> Constraints.mustMintValue val
                                            <> Constraints.mustPayToTheScript (UserDatum myKey (getADA userDatum) ((getMinted userDatum) - upamount)) (getValue o)
                            Contract.submitTxConstraintsWith lookups constraints >>= Contract.awaitTxConfirmed . Ledger.getCardanoTxId
    where
        isCDPOutput :: Ledger.PubKeyHash -> Ledger.ChainIndexTxOut -> Bool
        isCDPOutput myk o = case (fromJust $ getDatum @CDPDatum o) of
            ManagerDatum md -> False
            UserDatum cd _ _ -> cd == myk

type InitSchema = Contract.Endpoint "Init"     ()

type CDPSchema = Contract.Endpoint "Open"     UserParams
    Contract..\/ Contract.Endpoint "Deposit"  UserParams
    Contract..\/ Contract.Endpoint "Withdraw" UserParams
    Contract..\/ Contract.Endpoint "Mint"     UserParams
    Contract..\/ Contract.Endpoint "Burn"     UserParams
            
initEndpoint :: Contract.Contract (Last Value.Value) InitSchema Contract.ContractError ()
initEndpoint = Contract.selectList [init'] <> initEndpoint
  where
    init'     = Contract.endpoint @"Init"     $ \_ -> Contract.handleError Contract.logError $ Main.init >>= Contract.tell . Last . Just

myEndpoints :: Contract.Contract UserParams CDPSchema Contract.ContractError ()
myEndpoints = Contract.selectList [open', deposit', withdraw', mint', burn'] >> myEndpoints
  where   
    open'     = Contract.endpoint @"Open"     $ \a -> Contract.handleError Contract.logError $ open a
    deposit'  = Contract.endpoint @"Deposit"  $ \a -> Contract.handleError Contract.logError $ deposit a
    withdraw' = Contract.endpoint @"Withdraw" $ \a -> Contract.handleError Contract.logError $ withdraw a
    mint'     = Contract.endpoint @"Mint"     $ \a -> Contract.handleError Contract.logError $ mint a
    burn'     = Contract.endpoint @"Burn"     $ \a -> Contract.handleError Contract.logError $ burn a

main :: IO ()
main = Trace.runEmulatorTraceIO $ do
  v1 <- Trace.activateContractWallet (knownWallet 1) initEndpoint
  v2 <- Trace.activateContractWallet (knownWallet 2) myEndpoints
  
  Trace.callEndpoint @"Init" v1 ()
  void $ Trace.waitNSlots 1

  p <- getCDPParam v1
  
  Trace.callEndpoint @"Open" v2 $ UserParams p 0
  void $ Trace.waitNSlots 1
  return ()

  where
    getCDPParam :: Trace.ContractHandle (Last Value.Value) InitSchema Contract.ContractError -> Trace.EmulatorTrace Value.Value 
    getCDPParam h = do
      Trace.observableState h >>= \case
        Last (Just p) -> return p
        Last _        -> Trace.waitNSlots 1 >> getCDPParam h 
