

import Prelude (($!))

import Control.Monad (sequence_)

import Data.Int (Int)
import Data.List (replicate, (++), length)
import GHC.Num ((+))
import Data.String (String)

import System.IO


import qualified Control.Monad.Except.AocCommon.Spec
import qualified Data.List.AocCommon.Spec
import qualified Data.String.AocCommon.Spec
import qualified AocCommon.AocMain.Spec

seperator :: Int -> String
seperator n = '\n':(replicate n '-') ++ "\n"

end :: Int -> String
end n = '\n':(replicate n '#')

tests :: [(String, IO ())]
tests = [
  ("AocCommon.AocMain", AocCommon.AocMain.Spec.test),
  ("Control.Monad.Except.AocCommon", Control.Monad.Except.AocCommon.Spec.test),
  ("Data.List.AocCommon", Data.List.AocCommon.Spec.test),
  ("Data.String.AocCommon", Data.String.AocCommon.Spec.test)
  ]


main :: IO ()
main =
  do
    sequence_ [ do {putStrLn $! (seperator (8+(length title))) ++ "Testing " ++ title; test;  putStrLn $! end 30 } | (title, test) <- tests]
