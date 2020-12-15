{-# LANGUAGE FlexibleContexts #-}
module AocCommon.AocMain (ELM,
                          runELM,
                          aocMain',
                          conditionalBindELM,
                          logELM,
                          logsELM,
                          logStringELM,
                          logStringsELM,
                          getLogELM,
                          isExceptionELM,
                          throwErrorELM,
                          throwErrorStringELM,
                          throwErrorStringsELM,
                          aocTasks',
                          aocParseFile,
                          logToELM) where

import Prelude (undefined, ($!), ($), (+), (-))

import Control.Applicative (Applicative(..))
import Control.Monad (Monad(..), (=<<), when)
import Control.Monad.Except (MonadError(..), Except(..), runExcept, throwError)
import Control.Monad.Writer.Strict (WriterT(..), runWriterT, Writer, tell)

import Data.Bool (Bool(..), otherwise, (&&), not)
import Data.Char (Char(..))
import Data.Either (Either(..), either, isLeft)
import Data.Either.Combinators (mapBoth, mapRight, fromLeft', fromRight')
import Data.Eq (Eq(..), (==))
import Data.Ord(Ord(..))
import Data.Function ((.), const, id, flip)
import Data.Int (Int(..))
import Data.List (length, replicate, null, takeWhile, dropWhile, map, (++), head, reverse)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup(..))
import Data.String (String(..))
import qualified Data.Text as Text (Text, pack, unpack, cons, snoc, singleton, unlines, zip, length, lines, null)
import qualified Data.Text.IO as Text (putStrLn, readFile)
import Data.Tuple (fst, snd, uncurry)

import Safe (at, atMay)

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (IO)

import Text.Read (Read(..), read)
import Text.Show (Show(..))



type ELM = WriterT [Text.Text] (Except [Text.Text])

runELM :: ELM a -> Either [Text.Text] (a, [Text.Text])
runELM = runExcept . runWriterT

logTextELM :: Text.Text -> ELM ()
logTextELM = tell . (:[])

logStringELM :: String -> ELM ()
logStringELM = logTextELM . Text.pack

logELM :: Text.Text -> ELM ()
logELM = logTextELM

logTextsELM :: [Text.Text] -> ELM ()
logTextsELM = tell

logStringsELM :: [String] -> ELM ()
logStringsELM = logTextsELM . map Text.pack

logsELM :: [Text.Text] -> ELM ()
logsELM = logTextsELM

logToELM :: ELM a -> Text.Text -> ELM a
logToELM a msg = a <* logELM msg

getLogELM :: ELM a -> [Text.Text]
getLogELM a = case runELM a of
  Left trace -> trace
  Right (_,log) -> log

isExceptionELM :: ELM a -> Bool
isExceptionELM = isLeft . runELM

throwErrorStringELM :: String -> ELM a
throwErrorStringELM = throwError . (:[]) . Text.pack

throwErrorStringsELM :: [String] -> ELM a
throwErrorStringsELM = throwError . (map Text.pack)

throwErrorELM :: ELM a -> Text.Text -> ELM a
throwErrorELM a = (throwErrorsELM a) . (:[])

throwErrorsELM :: ELM a -> [Text.Text] -> ELM a
throwErrorsELM = throwErrorsELM' mempty

throwErrorsELM' pre a = (throwError :: [Text.Text] -> ELM a) . (pre `mappend` getLogELM a `mappend`)

conditionalBindELM :: Text.Text -> Text.Text -> Text.Text -> ELM a -> (a -> ELM b) -> ELM b
conditionalBindELM beforeMsg errMsg successMsg a f = if (not . isExceptionELM) a && isExceptionELM c
                                                     then throwErrorsELM' ((getLogELM a) `mappend` [beforeMsg]) c [errMsg]
                                                     else logToELM c successMsg
  where
    a' = a <* logELM beforeMsg
    c = a' >>= f


conditionalBindELM' :: Text.Text -> Text.Text -> Text.Text -> (a -> ELM b) -> ELM a -> ELM b
conditionalBindELM' beforeMsg errMsg successMsg = flip (conditionalBindELM beforeMsg errMsg successMsg)

report :: ELM a -> IO a
report e = do
  let run = runELM e
  case run of
      Left trace -> (Text.putStrLn . Text.unlines) trace >> exitFailure
      Right (inst, log) -> (Text.putStrLn . Text.unlines) log >> return inst

aocMain' :: (Text.Text -> ELM inst) -> (inst -> Text.Text) -> (inst -> ELM sol1) -> (sol1 -> Text.Text) -> (inst -> ELM sol2) -> (sol2 -> Text.Text) -> IO ()
aocMain' fromText toText solve1 toText1 solve2 toText2 = do
  content <- aocGetArgs'
  inst <- aocParseFile' fromText toText content
  aocRun' solve1 toText1 solve2 toText2 inst

aocGetArgs' :: IO Text.Text
aocGetArgs' = do
  args <- getArgs
  when (null args) $! do
      (Text.putStrLn . Text.pack) "No argument given."
      exitFailure
  let file = args `at` 0
  Text.readFile file

aocParseFile :: (Text.Text -> ELM inst) -> (inst -> Text.Text) -> Text.Text -> ELM inst
aocParseFile fromText toText content = check parsed
  where
    parse = conditionalBindELM' (Text.pack "Attempting to parse file.") (Text.pack "Something went wrong while parsing file.") (Text.pack "Successfully parsed file.") fromText
    parsed = (parse . return) content
    check = conditionalBindELM' (Text.pack "Checking if parsed file equals content of file.") (Text.pack "Parsed instance and content of file differ.") (Text.pack "Parsed instance and content of file match.") check'
    check' i = if parsedLines' == contentLines
      then return i
      else throwError $! (map Text.pack) msg
      where
        parsed' :: Text.Text
        parsed' = toText i
        parsedLines' = Text.lines parsed'
        contentLines = (reverse . dropWhile Text.null . reverse . Text.lines) content
        differenceLine :: [Text.Text] -> [Text.Text] -> Int -> (String, Int, Int, Maybe Char, Maybe Char)
        differenceLine [] [] lc = ([], lc, 0, Nothing, Nothing)
        differenceLine (l:ls) (j:js) lc
          | l == j = differenceLine ls js (lc + 1)
          | otherwise = (map fst match, lc, divergeAt, (Text.unpack l) `atMay` (divergeAt - 1), (Text.unpack j) `atMay` (divergeAt - 1))
          where
            zips = Text.zip l j
            match = takeWhile (uncurry (==)) zips
            diverge = dropWhile (uncurry (==)) zips
            divergeAt = length match + 1
        (matching, lineCount, charCount, firstDivergentCharOrig, firstDivergentCharParsed) = differenceLine contentLines parsedLines' 1
        preMsg :: String
        preMsg = "Parsed diverges at line: " ++ (show lineCount) ++ " character: " ++ (show charCount) ++ ""
        msg :: [String]
        msg = [preMsg, matching, pointer, orig, pars]
          where
            pointer = replicate (length matching) ' ' ++ "^ here"
            orig = matching ++ maybe " | line ends here" (:[]) firstDivergentCharOrig ++ " <-- should be."
            pars = matching ++ maybe " | line ends here" (:[]) firstDivergentCharParsed ++ " <-- is."




aocParseFile' :: (Text.Text -> ELM inst) -> (inst -> Text.Text) -> Text.Text -> IO inst
aocParseFile' fromText toText = report . aocParseFile fromText toText

aocRun' :: (inst -> ELM sol1) -> (sol1 -> Text.Text) -> (inst -> ELM sol2) -> (sol2 -> Text.Text) -> inst -> IO ()
aocRun' solve1 toText1 solve2 toText2 inst = do
  let run = (runELM . aocTasks' solve1 solve2) inst
  case run of
    Right ((sol1, sol2), log) -> do
      (Text.putStrLn . Text.unlines) log
      Text.putStrLn (Text.pack (replicate 35 '#'))
      (Text.putStrLn . toText1) sol1
      (Text.putStrLn . toText2) sol2
      exitSuccess
    Left trace -> do
      (Text.putStrLn . Text.unlines) trace
      exitFailure


aocTasks' :: (inst -> ELM sol1) -> (inst -> ELM sol2) -> inst -> ELM (sol1,sol2)
aocTasks' solve1 solve2 inst = do
  sol1 <- solve1 inst
  sol2 <- solve2 inst
  return (sol1, sol2)
