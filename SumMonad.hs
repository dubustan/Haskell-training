module SumMonad where
import Control.Monad
data Sum a b = First a | Second b deriving (Eq, Show)
instance Functor (Sum a) where
 fmap _ (First a) = First a
 fmap f (Second b) = Second (f b)
instance Applicative (Sum a) where
 pure b = Second b 
 (<*>) (Second x) (Second y) = Second (x y)
 (<*>) (First x) _ = (First x)
 (<*>) _ (First x) = (First x)
  
instance Monad (Sum a) where
 return = pure
 (>>=) (First a) _ = (First a)
 (>>=) (Second b) f = f b

