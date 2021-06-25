module Chap21 where 
import Data.Monoid
import Data.Foldable
import Control.Applicative
newtype Identity' a = Identity' a deriving (Eq, Ord, Show)
instance Functor Identity' where
 fmap f (Identity' a) = Identity' (f a)
instance Foldable Identity' where
 foldMap f (Identity' a) = f a 
instance Traversable Identity' where
 traverse f (Identity' a) = Identity' <$> (f a)
 
 
newtype Constant' a b =Constant' { getConstant :: a }
instance Functor (Constant' a) where 
 fmap f (Constant' a) = Constant' a 
instance Foldable (Constant' a) where 
 foldMap _ _ = mempty
instance Traversable (Constant' a) where
 traverse f (Constant' a) = pure $ Constant' a
 
 
data Optional a = Nada | Yep a
instance Functor Optional where 
 fmap f Nada = Nada
 fmap f (Yep a) = Yep (f a)
instance Foldable Optional where 
 foldMap f Nada = mempty
 foldMap f (Yep a) = f a
instance Traversable Optional where
 traverse f Nada = pure Nada
 traverse f (Yep a) = Yep <$> f a
 

data List a = Nil | Cons a (List a)
instance Functor List where 
 fmap f Nil = Nil
 fmap f (Cons a as) = Cons (f a) (fmap f as)
instance Foldable List where 
 foldMap f Nil = mempty
 foldMap f (Cons a as) = f a <> (foldMap f as)
instance Traversable List where 
 traverse _ Nil = pure Nil
 traverse f (Cons x xs) = Cons <$> f x <*> (traverse f xs)
 

data Three a b c = Three a b c
instance Functor (Three a b) where
 fmap f (Three a b c) = Three a b (f c)
instance Foldable (Three a b) where
 foldMap f (Three a b c) = f c
instance Traversable (Three a b) where 
 traverse f (Three a b c) = (Three a b) <$> (f c)

data Pair a b = Pair a b
instance Functor (Pair a) where
 fmap f (Pair a b) = Pair a (f b)
instance Foldable (Pair a) where 
 foldMap f (Pair a b) = f b
instance Traversable (Pair a) where
 traverse f (Pair a b) = (Pair a) <$> (f b)
 
data Big a b = Big a b b
instance Functor (Big a) where 
 fmap f (Big a b c) = Big a (f b) (f c)
instance Foldable (Big a) where 
 foldMap f (Big a b c) = (f b) <> (f c)
instance Traversable (Big a) where 
 traverse f (Big a b c) = (Big a) <$> (f b) <*> (f c)

data Bigger a b = Bigger a b b b 
instance Functor (Bigger a) where 
 fmap f (Bigger a b c d) = Bigger a (f b) (f c) (f d) 
instance Foldable (Bigger a) where 
 foldMap f (Bigger a b c d) = (f b) <> (f c) <> (f d)
instance Traversable (Bigger a) where
 traverse f (Bigger a b c d) = (Bigger a) <$> (f b) <*> (f c) <*> (f d)
 
data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
instance Functor Tree where 
 fmap f Empty = Empty
 fmap f (Leaf a) = Leaf (f a)
 fmap f (Node le x ri) = Node (fmap f le) (f x) (fmap f ri)
instance Foldable Tree where 
 foldMap f Empty = mempty
 foldMap f (Leaf a) = f a
 foldMap f (Node le x ri) = (foldMap f le) <> (f x) <> (foldMap f ri)
instance Traversable Tree where 
 traverse _ Empty = pure Empty
 traverse f (Leaf a) = Leaf <$> f a
 traverse f (Node le x ri) = liftA3 Node (traverse f le) (f x) (traverse f ri)
