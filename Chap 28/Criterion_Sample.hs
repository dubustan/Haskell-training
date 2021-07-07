{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
--import System.Random
--import Control.Parallel.Strategies
--import Control.Monad.Trans.State
--import Control.Monad
--import Control.Applicative
--import Text.Trifecta hiding (parseTest)
--import Data.ByteString
--import Data.Ratio ((%))
--import Data.String (IsString)
--import qualified Data.Attoparsec.ByteString as A
--import Data.Attoparsec.ByteString (parseOnly)
--import Data.ByteString (ByteString) 
import Text.Parsec (Parsec, parseTest)
import Criterion.Main

infixl 9 !?
_      !? n | n < 0 = Nothing
[]     !? _         = Nothing
(x:_)  !? 0         = Just x
(_:xs) !? n         = xs !? (n - 1)
 
myList :: [Int]
myList = [1..9999]

main :: IO ()
main = defaultMain
  [ bench "index list 9999"
   $ whnf (myList !!) 9998
   , bench "index list maybe index 999"
   $ whnf (myList !?) 9998
  ]
