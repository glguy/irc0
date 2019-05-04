{-# Language BlockArguments, RankNTypes, GeneralizedNewtypeDeriving #-}
module WithIO where

import           Control.Monad.Codensity
import           Control.Monad.IO.Class (MonadIO)
import           Data.Text              (Text)
import           Foreign                (Storable, Ptr, withArray)
import           Foreign.C              (CSize, CString)
import qualified Data.Text.Foreign as Text

-- | Continuation-passing style bracked IO actions.
newtype WithIO a = WithIO (Codensity IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Return the bracket IO action.
evalWithIO :: WithIO a -> IO a
evalWithIO (WithIO m) = lowerCodensity m

withIO :: (forall r. (a -> IO r) -> IO r) -> WithIO a
withIO f = WithIO (Codensity f)

exportText :: Text -> WithIO (CString, CSize)
exportText txt =
  do (ptr,len) <- withIO (Text.withCStringLen txt)
     return (ptr, fromIntegral len)

exportArray :: Storable a => [a] -> WithIO (Ptr a)
exportArray xs = withIO (withArray xs)
