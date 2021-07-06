{-# LANGUAGE InstanceSigs #-}
module Main where
import System.Random
import Control.Parallel.Strategies
import Control.Monad.Trans.State
import Control.Monad
import Control.Applicative
import Text.Trifecta
import Data.Ratio ((%))

parseFraction :: Parser Rational
parseFraction = do 
 numerator <- decimal
 char '/'
 denominator <- decimal
 return (numerator % denominator)

badFraction = "1/0"
alsoBad = "10"
shouldWork = "10/2"
shouldAlsoWork = "2/1"

main :: IO ()
main = do
 let parseFraction' = parseString parseFraction mempty
 print $ parseFraction' badFraction
 print $ parseFraction' shouldWork
 print $ parseFraction' shouldAlsoWork
 print $ parseFraction' alsoBad
