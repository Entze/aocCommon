module AocCommon.AocMain where

import Prelude (undefined)

import Control.Monad (Monad(..), (=<<))
import Control.Monad.Except (MonadError(..))

import Data.Bool (Bool(..))
import Data.Either (Either(..), either)
import Data.Either.Combinators (mapBoth, mapRight)
import Data.Eq (Eq(..))
import Data.Function ((.), const, id, flip)
import Data.List (length, replicate)
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup(..))
import qualified Data.Text as Text (Text, pack, cons, snoc, singleton,unlines)
import qualified Data.Text.IO as Text (putStrLn, readFile)
import Data.Tuple (fst, snd)

import Safe (at)

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (IO)

import qualified AocCommon.AocDay as AocDay (AocDay (..))
import qualified AocCommon.AocTask as AocTask (AocTask (..))


aocMain :: (AocDay.AocDay inst, AocTask.AocTask solution1, AocTask.AocTask solution2) => (Text.Text -> inst) -> (inst -> Either [Text.Text] (solution1, [Text.Text])) -> (inst -> Either [Text.Text] (solution2, [Text.Text])) -> IO ()
aocMain fromText solve1 solve2 = do
  args <- getArgs
  case length args of
    0 -> do
      (Text.putStrLn . Text.pack) "No argument given."
      exitFailure
  let file = args `at` 0
  content <- Text.readFile file
  let inst = fromText content
  case AocDay.instanceToText inst == content of
    False -> do
      (Text.putStrLn . Text.pack) "Parsed instance and content of file differ."
      exitFailure
  let result = aocTasks solve1 solve2 inst
  case result of
    Right ((sol1, sol2),log) -> do
      (Text.putStrLn . Text.unlines) log
      Text.putStrLn (Text.pack (replicate 35 '#'))
      (Text.putStrLn . AocTask.solutionToText) sol1
      (Text.putStrLn . AocTask.solutionToText) sol2
      exitSuccess
    Left err -> do
      (Text.putStrLn . Text.unlines) err
      exitFailure


aocTasks :: (AocDay.AocDay inst, AocTask.AocTask solution1, AocTask.AocTask solution2) => (inst -> Either [Text.Text] (solution1, [Text.Text])) -> (inst -> Either [Text.Text] (solution2, [Text.Text])) -> inst -> Either [Text.Text] ((solution1, solution2), [Text.Text])
aocTasks solve1 solve2 inst = combine (,) ([Text.pack (replicate 25 '-')]) res1 res2
  where
    combine :: Semigroup e => (a -> b -> c) -> e -> Either e (a,e) -> Either e (b,e) -> Either e (c,e)
    combine f sep (Right (a,l1)) (Right (b,l2)) = Right (f a b, l1 <> sep <> l2)
    combine f sep (Left e1) (Left e2) = Left (e1 <> sep <> e2)
    combine _ sep (Left e) (Right (_, l2)) = Left (e <> sep <> l2)
    combine _ sep (Right (_, l1)) (Left e) = Left (l1 <> sep <> e)
    res1 = solve1 inst
    res2 = solve2 inst
