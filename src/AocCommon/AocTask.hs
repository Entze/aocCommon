module AocCommon.AocTask(
  AocTask (..)
                        )where

import Prelude ()

import Data.Either(Either)
import qualified Data.Text as Text

import AocCommon.AocDay (AocDay)

class AocTask solution where
  solve :: AocDay inst => inst -> Either [Text.Text] (solution, [Text.Text])
  solutionToText :: solution -> Text.Text
