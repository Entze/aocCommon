module AocCommon (
  gnomeSortBy,
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
import Data.Ord (Ord, Ordering (GT), compare)


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


gnomeSortBy :: Ord a => (a -> a -> Ordering) -> [a] -> [a]
gnomeSortBy o list@(l1:l2:ls)
  | l' `o` l'' == GT = l'':(gnomeSortBy o (l':ls''))
  | otherwise = l':list''
  where
    list''@(l'':ls'') = gnomeSortBy o ls'
    (l':ls')
      | l1 `o` l2 == GT = l2:l1:ls
      | otherwise = list

gnomeSortBy _ x = x
