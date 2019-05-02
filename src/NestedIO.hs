{-# Language BlockArguments, RankNTypes #-}
module NestedIO where

import Control.Monad

-- | Continuation-passing style bracked IO actions.
newtype NestedIO a = NestedIO { runNestedIO :: forall r. (a -> IO r) -> IO r }

instance Functor NestedIO where
  fmap = liftM

instance Applicative NestedIO where
  (<*>) = ap
  pure x = NestedIO \k -> k x

instance Monad NestedIO where
  m >>= f = NestedIO \k -> runNestedIO m \x -> runNestedIO (f x) k

-- | Return the bracket IO action.
evalNestedIO :: NestedIO a -> IO a
evalNestedIO (NestedIO m) = m return

-- | Wrap up a bracketing IO operation where the continuation takes 1 argument
nest1 :: (forall r. (a -> IO r) -> IO r) -> NestedIO a
nest1 = NestedIO

-- | Wrap up a bracketing IO operation where the continuation takes 2 argument
nest2 :: (forall r. (a -> b -> IO r) -> IO r) -> NestedIO (a,b)
nest2 f = NestedIO (f . curry)
