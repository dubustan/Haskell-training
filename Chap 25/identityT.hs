{-# LANGUAGE InstanceSigs #-}
module IdentityT where 
import Control.Applicative
import Control.Monad
newtype IdentityTT f a = IdentityTT {runIdentityTT :: (f a)} deriving (Show, Eq)

instance (Functor m) => Functor (IdentityTT m) where 
 fmap f (IdentityTT fa) = IdentityTT (fmap f fa)
 
instance (Applicative m) => Applicative (IdentityTT m) where 
 pure a = IdentityTT (pure a)
 (<*>) (IdentityTT fab) (IdentityTT fa) = IdentityTT (fab <*> fa)
 
instance Monad f => Monad (IdentityTT f) where 
 return = pure
 (IdentityTT ma) >>= f = IdentityTT $ ma >>= runIdentityTT . f
