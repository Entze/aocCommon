module Control.Monad.Except.AocCommon (
  maybeToError,
  maybeToErrorWith,
  ) where

import Prelude ()
import Control.Monad (Monad (return))
import Control.Monad.Except (MonadError (throwError))

import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid (Monoid (mempty))


maybeToError :: (Monoid e, MonadError e m) => Maybe a -> m a
maybeToError Nothing = throwError mempty
maybeToError (Just a) = return a

maybeToErrorWith :: MonadError e m => e -> Maybe a -> m a
maybeToErrorWith _ (Just a) = return a
maybeToErrorWith e Nothing = throwError e
