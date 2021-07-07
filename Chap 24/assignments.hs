{-# LANGUAGE InstanceSigs #-}
module Main where
import System.Random
import Control.Parallel.Strategies
import Control.Monad.Trans.State
import Control.Monad
import Control.Applicative
import Text.Trifecta
import Data.ByteString
import Data.Ratio ((%))

assignmentEx :: String
assignmentEx = "woot=1"

type Name = String
type Value = String

parseAssignment :: Parser (Name, Value)
parseAssignment = do 
 name <- some letter
 _ <- char '='
 val <- some (noneOf "\n")
 skipEOL
 return (name, val)
 
parseAssignment' :: Parser (Name, Value)
parseAssignment' = do
 name <- some letter
 _ <- char '='
 val <- some (noneOf "\n")
 return (name, val) 
 
skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

main = do 
 print "bangbeo"
