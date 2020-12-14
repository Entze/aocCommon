module AocCommon.AocDay (
  AocDay (..)
                        ) where

import Prelude ()

import Control.Monad (return)

import Data.Either (Either)
import qualified Data.Text as Text

class AocDay inst where
  instanceFromText :: Text.Text -> Either [Text.Text] inst
  instanceToText :: inst -> Text.Text
  reduceInstance :: inst -> Either [Text.Text] inst
  reduceInstance = return
