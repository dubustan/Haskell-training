{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import System.Random
import Control.Parallel.Strategies
import Control.Monad.Trans.State
import Control.Monad
import Control.Applicative
import Text.Trifecta
import Data.ByteString
import Data.Ratio ((%))
import Data.String (IsString)
import Data.Attoparsec.Text (parseOnly)

badFraction :: IsString s => s
badFraction = "1/0"
alsoBad :: IsString s => s
alsoBad = "10"

shouldWork :: IsString s => s
shouldWork = "1/2"

shouldAlsoWork :: IsString s => s
shouldAlsoWork = "2/1"

parseFraction :: (Monad m, TokenParsing m) => m Rational
parseFraction = do
 numerator <- decimal
 _ <- char '/'
 denominator <- decimal
 case denominator of
  0 -> fail "Denominator cannot be zero"
  _ -> return (numerator % denominator)

main = do 
 let attoP = parseOnly parseFraction
 print $ attoP badFraction
 print $ attoP shouldWork
 print $ attoP shouldAlsoWork
 print $ attoP alsoBad
 -- parseString is Trifecta
 let p f i = parseString f mempty i
 print $ p parseFraction badFraction
 print $ p parseFraction shouldWork
 print $ p parseFraction shouldAlsoWork
 print $ p parseFraction alsoBad
