module Chap19 where 
import Data.Monoid
import Data.Foldable
sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum 

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = getAny . foldMap (\y -> Any (y == x))

null' :: (Foldable t) => t a -> Bool
null' x = length x == 0

length' :: (Foldable t) => t a -> Int
length' = foldr (\_ x -> x + 1) 0

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\b a -> (f b) <> a) mempty
