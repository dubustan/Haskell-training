module Chap17 where
import Data.Monoid
import Control.Applicative (liftA3)
data Pair a = Pair a a deriving (Eq, Show)
instance Functor Pair where
 fmap f (Pair a a') = Pair (f a) (f a')
instance Applicative Pair where
 pure a = Pair a a 
 (<*>) (Pair f g) (Pair x y) = Pair (f x) (g y)
 

data Two a b = Two a b deriving (Eq, Show)
instance Functor (Two a) where
 fmap f (Two a b) = Two a (f b)
instance Monoid a => Applicative (Two a) where
 pure b = Two mempty b
 (<*>) (Two a b) (Two c d) = Two (a <> c) (b d)
 
 
data Three a b c = Three a b c deriving (Eq, Show)
instance Functor (Three a b) where
 fmap f (Three a b c) = Three a b (f c)
instance (Monoid a, Monoid b) => Applicative(Three a b) where
 pure c = Three mempty mempty c
 (<*>) (Three a b f) (Three a' b' x) = Three (a <> a') (b <> b') (f x)  
 
data Three' a b = Three' a b b deriving (Eq, Show)
instance Functor (Three' a) where
 fmap f (Three' a b c) = Three' a (f b) (f c)
instance Monoid a => Applicative (Three' a) where
 pure b = Three' mempty b b
 (<*>) (Three' a b c) (Three' a' b' c') = Three' (a <> a) (b b') (c c')
 
 
data Four a b c d = Four a b c d deriving (Eq, Show)
instance Functor (Four a b c) where
 fmap f (Four a b c d) = Four a b c (f d)
instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
 pure d = Four mempty mempty mempty d
 (<*>) (Four a b c d) (Four a' b' c' d') = Four (a <> a') (b <> b') (c <> c') (d d')
 
data Four' a b = Four' a a a b deriving (Eq, Show) 
instance Functor (Four' a) where
 fmap f (Four' a b c d) = Four' a b c (f d)
instance Monoid a => Applicative (Four' a) where
 pure b = Four' mempty mempty mempty b
 (<*>) (Four' a b c d) (Four' a' b' c' d') = Four' (a <> a') (b <> b') (c <> c') (d d')
 
 -- combinations

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combinations :: [(Char, Char, Char)]
combinations = liftA3 (,,) stops vowels stops
