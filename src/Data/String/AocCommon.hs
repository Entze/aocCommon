module Data.String.AocCommon where

import Prelude ()

import Data.Char (Char)
import Data.Foldable (foldr')
import Data.Function ((.))
import Data.List ((++), drop)
import Data.String (String)

joinStringsBy :: Char -> [String] -> String
joinStringsBy c = drop 1 . foldr' ((++) . (c:)) ""
