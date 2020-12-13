module Data.List.AocCommon (
  gnomeSortBy,
  iterateUntilFixpoint,
  iterateUntilFixpoint',
  takeUntilEqual
  ) where

import Prelude ()

import Data.Bool (otherwise)
import Data.Eq (Eq ((==)))
import Data.Function ((.))
import Data.List (iterate, iterate')
import Data.Maybe (Maybe(Just, Nothing))
import Data.Ord (Ord, Ordering (GT), compare)


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
