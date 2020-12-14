module CommonSpec where

import Safe

terminates :: (a -> b) -> a -> Bool
terminates = terminates1

terminates1 :: (a -> b) -> a -> Bool
terminates1 f a = const True $! f a

terminates2 :: (a -> b -> c) -> a -> b -> Bool
terminates2 f a b = const True $! f a b

isStable :: Eq a => (a -> [a]) -> a -> Bool
isStable f a = end == (end >>= (lastMay . f))
  where
    end = lastMay a'
    a' = f a
