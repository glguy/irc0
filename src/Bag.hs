{-# Language TypeFamilies, DeriveTraversable #-}
module Bag
  ( Bag, Key(..), empty, insert, delete, update, isEmpty ) where

import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import Control.Lens

newtype Key = Key Int

data Bag a = Bag !Int (IntMap a)
  deriving (Functor, Foldable, Traversable)

empty :: Bag a
empty = Bag 1 IntMap.empty

isEmpty :: Bag a -> Bool
isEmpty (Bag i _) = i == 1

insert :: a -> Bag a -> (Key, Bag a)
insert x (Bag next m) = (Key next, Bag (next+1) (IntMap.insert next x m))

delete :: Key -> Bag a -> Bag a
delete (Key i) (Bag next m) = Bag next (IntMap.delete i m)

update :: Key -> (a -> a) -> Bag a -> Bag a
update (Key i) f (Bag next m) = Bag next (IntMap.update (Just . f) i m)

type instance Index (Bag a) = Key
type instance IxValue (Bag a) = a

instance Ixed (Bag a) where
  ix (Key i) f (Bag next m) = Bag next <$> ix i f m
