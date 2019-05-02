{-# Language BlockArguments, RankNTypes #-}
module WithIO where

import           Control.Monad          (ap, liftM)
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Data.Text              (Text)
import           Foreign                (Storable, Ptr, withArray)
import           Foreign.C              (CSize, CString)
import qualified Data.Text.Foreign as Text

-- | Continuation-passing style bracked IO actions.
newtype WithIO a = WithIO { runWithIO :: forall r. (a -> IO r) -> IO r }

instance Functor WithIO where
  fmap = liftM

instance Applicative WithIO where
  (<*>)  = ap
  pure x = WithIO \k -> k x

instance Monad WithIO where
  m >>= f = WithIO \k -> runWithIO m \x -> runWithIO (f x) k

instance MonadIO WithIO where
  liftIO io = WithIO (io >>=)

-- | Return the bracket IO action.
evalWithIO :: WithIO a -> IO a
evalWithIO (WithIO m) = m return

exportText :: Text -> WithIO (CString, CSize)
exportText txt =
  do (ptr,len) <- WithIO (Text.withCStringLen txt)
     return (ptr, fromIntegral len)

exportArray :: Storable a => [a] -> WithIO (Ptr a)
exportArray xs = WithIO (withArray xs)
