{-# LANGUAGE InstanceSigs #-}
module Main where
import System.Random
import Control.Parallel.Strategies
import Control.Monad.Trans.State
import Control.Monad
import Control.Applicative
import Text.Trifecta
import Data.Ratio ((%))

type NumberOrString = Either Integer String
a = "blah"
b = "123"
c = "123blah789"

parseNos :: Parser NumberOrString
parseNos = (Left <$> integer) <|> (Right <$> some letter)

main = do 
 let p f i = parseString f mempty i
 print $ p (some letter) a
 print $ p integer b
 print $ p parseNos a
 print $ p parseNos b
 print $ p (many parseNos) c
 print $ p (some parseNos) c
