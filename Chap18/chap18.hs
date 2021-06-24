module Chap18 where
import Control.Monad
data Nope a = NopeDotJpg deriving (Eq, Show)
instance Functor Nope where
 fmap _ NopeDotJpg = NopeDotJpg
instance Applicative Nope where 
 pure _ = NopeDotJpg
 (<*>) _ _ = NopeDotJpg
instance Monad Nope where
 return = pure
 (>>=) _ _ = NopeDotJpg
 
 
data PhhhbbtttEither b a = Left' a | Right' b deriving (Eq, Show)
instance Functor (PhhhbbtttEither b) where
 fmap f (Left' a) = Left' (f a)
 fmap f (Right' b) = Right' b
instance Applicative (PhhhbbtttEither b) where
 pure = Left'
 (<*>) (Left' f) (Left' x) = Left' (f x)
 (<*>) (Right' f) _ = Right' f
 (<*>) _ (Right' f) = Right' f
instance Monad (PhhhbbtttEither b) where 
 return = pure 
 (>>=) (Right' a) _ = Right' a
 (>>=) (Left' a) f = f a 
 

newtype Identity a = Identity a deriving (Eq, Ord, Show)
instance Functor Identity where
 fmap f (Identity a) = Identity (f a)
instance Applicative Identity where
 pure = Identity
 (<*>) (Identity f) (Identity x) = Identity (f x)
instance Monad Identity where
 return = pure
 (>>=) (Identity x) f = f x
 
data List a = Nil | Cons a (List a)
append :: List a -> List a -> List a
append Nil x = x
append (Cons a as) x = Cons a $ append as x
instance Functor List where
 fmap f Nil = Nil
 fmap f (Cons a as) = Cons (f a) (fmap f as) 
instance Applicative List where 
 pure x = Cons x Nil
 (<*>) Nil _ = Nil
 (<*>) _ Nil = Nil
 (<*>) (Cons f fs) xs = append (fmap f xs) (fs <*> xs)
instance Monad List where
  return = pure
  (>>=) Nil _ = Nil
  (>>=) (Cons a Nil) f = f a
  (>>=) (Cons a rest) f = f a `append` (rest >>= f)
  
j :: Monad m => m (m a) -> m a
j = join

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = liftM

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

a :: Monad m => m a -> m (a -> b) -> m b
a = flip ap

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = liftM2 (++) ((:[]) <$> (f x)) (meh xs f)

flipType :: (Monad m) => [m a] -> m [a]
flipType xs = meh xs id
