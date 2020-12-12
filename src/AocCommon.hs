module AocCommon (
  maybeToError,
  maybeToErrorWith,
  iterateUntilFixpoint,
  iterateUntilFixpoint',
  takeUntilEqual
  ) where

import Prelude ()
import Control.Monad (Monad (return))
import Control.Monad.Except (MonadError (throwError))

import Data.Bool (otherwise)
import Data.Eq (Eq ((==)))
import Data.Function ((.))
import Data.List (iterate, iterate')
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid (Monoid (mempty))


maybeToError :: (Monoid e, MonadError e m) => Maybe a -> m a
maybeToError Nothing = throwError mempty
maybeToError (Just a) = return a

maybeToErrorWith :: MonadError e m => e -> Maybe a -> m a
maybeToErrorWith _ (Just a) = return a
maybeToErrorWith e Nothing = throwError e


iterateUntilFixpoint :: Eq a => (a -> a) -> a -> [a]
iterateUntilFixpoint f = takeUntilEqual . iterate f

iterateUntilFixpoint' :: Eq a => (a -> a) -> a -> [a]
iterateUntilFixpoint' f = takeUntilEqual . iterate' f

takeUntilEqual :: Eq a => [a] -> [a]
takeUntilEqual (a:(ls@(b:_)))
  | a == b = [a]
  | otherwise = a:(takeUntilEqual ls)
takeUntilEqual x = x