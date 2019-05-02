{-# Language TemplateHaskell #-}
module HookMap
  ( HookMap
  , empty
  , remove
  , insert
  , HookMap.lookup
  ) where

import Control.Lens
import Data.Map (Map)
import Data.IntMap (IntMap)
import qualified Data.Map as Map

data HookMap k a = HookMap
  { _nextKey :: Int
  , _entries :: Map k (IntMap (IntMap a))
     -- event-name priority hook-id
  }

makeLenses ''HookMap

empty :: HookMap k a
empty = HookMap
  { _nextKey = 1
  , _entries = Map.empty
  }

remove :: Int -> HookMap k a -> HookMap k a
remove i m = m & entries . traverse . traverse %~ sans i

insert :: Ord k => Int -> k -> a -> HookMap k a -> (Int, HookMap k a)
insert priority key value m = (i, m2)
  where
    (i, m1) = m & nextKey <<+~ 1
    m2 = m1 & entries . at key . non' _Empty . at priority . non' _Empty . at i ?~ value

lookup :: Ord k => k -> HookMap k a -> [a]
lookup k = toListOf (entries . ix k . folded . folded)
