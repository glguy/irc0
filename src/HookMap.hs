{-# Language ScopedTypeVariables, TemplateHaskell #-}
module HookMap
  ( HookMap
  , empty
  , remove
  , insert
  , HookMap.lookup
  ) where

import Control.Lens
import Data.Foldable (toList)
import Data.Map (Map)
import Data.IntMap (IntMap)
import qualified Data.Map as Map
import qualified Bag
import Bag (Bag)

newtype HookMap k a = HookMap { unHookMap :: Map k (IntMap (Bag a)) }

empty :: HookMap k a
empty = HookMap Map.empty

access :: (Functor f, Ord k) => k -> Int -> LensLike' f (HookMap k a) (Bag a)
access key priority f (HookMap m) =
  HookMap <$> (at key . non' _Empty . at priority . anon Bag.empty Bag.isEmpty) f m

remove :: Ord k => k -> Int -> Bag.Key -> HookMap k a -> HookMap k a
remove key priority i m = m & access key priority %~ Bag.delete i

insert :: Ord k => Int -> k -> a -> HookMap k a -> (Bag.Key, HookMap k a)
insert priority key value m = m & access key priority %%~ Bag.insert value

lookup :: forall k a. Ord k => k -> [HookMap k a] -> [a]
lookup k xs = toListOf (folded . folded) tmp
  where
    tmp :: IntMap [a]
    tmp = foldMapOf (folded . to unHookMap . ix k) (fmap toList) xs
