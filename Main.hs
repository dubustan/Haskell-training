module Main where
import System.Random
import Control.Parallel.Strategies
import Control.Monad.Trans.State
import Control.Monad
import Control.Applicative

data Die = DieOne | DieTwo | DieThree | DieFour | DieFive | DieSix deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n = 
 case n of 
  1 -> DieOne
  2 -> DieTwo
  3 -> DieThree
  4 -> DieFour
  5 -> DieFive
  6 -> DieSix
  x -> error $ "intToDie got non 1-6 integer: " ++ show x
  
rollDie :: State StdGen Die
rollDie = state $ do
 (n, s) <- randomR (1, 6)
 return (intToDie n, s)
 
rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))
rollDieThreeTime' :: State StdGen (Die, Die, Die)
rollDieThreeTime' = liftA3 (,,) rollDie rollDie rollDie 
infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie
nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g
 where 
  go :: Int -> Int -> StdGen -> Int
  go sum count gen 
   | sum >= n = count
   | otherwise = 
     let (die, nextGen) = randomR (1, 6) gen
     in go (sum + die) (count + 1) nextGen

main = do
 putStrLn "bangbeo"
