{-# LANGUAGE InstanceSigs #-}
module Main where
import System.Random
import Control.Parallel.Strategies
import Control.Monad.Trans.State
import Control.Monad
import Control.Applicative

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put s = State (\_ -> ((), s))

exec :: State s a -> s -> s
exec (State sa) s = snd (sa s)

eval :: State s a -> s -> a
eval (State sa) s = fst $ sa s

modify :: (s -> s) -> State s ()
modify f = State (\s -> ((), f s))

main = do
 putStrLn "bangbeo"
