module AocCommon.AocMain.Spec where


import Test.Hspec
--import Test.Hspec.LeanCheck as LC
--import Test.Hspec.QuickCheck
--import Test.QuickCheck as QC hiding ((===))
--import Test.LeanCheck.Utils.Operators
--import Safe

import Control.Monad.Except
import qualified Data.Text as Text

--import CommonSpec
import AocCommon
import AocCommon.AocMain

newtype TestInstance = Inst Int deriving (Eq, Ord, Show, Read)
newtype TestSolution1 = Sol1 Int deriving (Eq, Ord, Show, Read)
newtype TestSolution2 = Sol2 Int deriving (Eq, Ord, Show, Read)

testFromTextSuccess :: Text.Text -> ELM TestInstance
testFromTextSuccess t = logToELM ((return . read . Text.unpack) t) (Text.pack "Succesfully done \"fromText\".")

testFromTextFailure :: Text.Text -> ELM TestInstance
testFromTextFailure _ = (throwError . (:[]) . Text.pack) "Error in \"fromText\"."

testToTextSuccess :: TestInstance -> Text.Text
testToTextSuccess = Text.pack . show

testToTextFailure :: TestInstance -> Text.Text
testToTextFailure (Inst n) = (Text.pack . show . Inst) (n+1)

testSolve1Success :: TestInstance -> ELM TestSolution1
testSolve1Success (Inst n) = logToELM ((return . Sol1 . (+1)) n) (Text.pack "Success in \"solve1\".")

testSolve1Failure :: TestInstance -> ELM TestSolution1
testSolve1Failure _ = ((throwErrorELM (return (Sol1 0))) . Text.pack) "Error in \"solve1\"."

testSolve2Success :: TestInstance -> ELM TestSolution2
testSolve2Success (Inst n) = logToELM ((return . Sol2 . (+2)) n) (Text.pack "Success in \"solve2\".")

testSolve2Failure :: TestInstance -> ELM TestSolution2
testSolve2Failure _ = ((throwErrorELM (return (Sol2 0))) . Text.pack) "Error in \"solve2\"."

test :: IO ()
test = hspec $ do
  describe "aocParseFile :: (Text.Text -> ELM inst) -> (inst -> Text.Text) -> Text.Text -> ELM inst" $ do
    context "aocParseFile :: (Text.Text -> ELM TestInstance) -> (TestInstance -> Text.Text) -> Text.Text -> ELM TestInstance" $ do
      it "toELM . aocParseFile testFromTextSuccess testToTextSuccess (Text.pack \"2\") ->> Right (2, _)" $
        (runELM . aocParseFile testFromTextSuccess testToTextSuccess) (Text.pack "Inst 2") `shouldBe` Right (Inst 2, map Text.pack [
                                                                                                                      "Attempting to parse file.",
                                                                                                                      "Succesfully done \"fromText\".",
                                                                                                                      "Successfully parsed file.",
                                                                                                                      "Checking if parsed file equals content of file.",
                                                                                                                      "Parsed instance and content of file match."
                                                                                                                     ])
      it "toELM . aocParseFile testFromTextSuccess testToTextFailure (Text.pack \"2\") ->> Left _" $
        (runELM . aocParseFile testFromTextSuccess testToTextFailure) (Text.pack "Inst 2\n") `shouldBe` Left (map Text.pack [
                                                                                                               "Attempting to parse file.",
                                                                                                               "Succesfully done \"fromText\".",
                                                                                                               "Successfully parsed file.",
                                                                                                               "Checking if parsed file equals content of file.",
                                                                                                               "Parsed diverges at line: 1 character: 6",
                                                                                                               "Inst ",
                                                                                                               "     ^ here",
                                                                                                               "Inst 2 <-- should be.",
                                                                                                               "Inst 3 <-- is.",
                                                                                                               "Parsed instance and content of file differ."])
      it "toELM . aocParseFile testFromTextFailure testToTextSuccess (Text.pack \"2\") ->> Left _" $
        (runELM . aocParseFile testFromTextFailure testToTextSuccess) (Text.pack "Inst 2") `shouldBe` Left (map Text.pack [
                                                                                                               "Attempting to parse file.",
                                                                                                               "Error in \"fromText\".",
                                                                                                               "Something went wrong while parsing file."
                                                                                                               ])
      it "toELM . aocParseFile testFromTextFailure testToTextFailure (Text.pack \"2\") ->> Left _" $
        (runELM . aocParseFile testFromTextFailure testToTextFailure) (Text.pack "Inst 2") `shouldBe` Left (map Text.pack [
                                                                                                               "Attempting to parse file.",
                                                                                                               "Error in \"fromText\".",
                                                                                                               "Something went wrong while parsing file."
                                                                                                               ])
  describe "aocTasks' :: (inst -> ELM sol1) -> (inst -> ELM sol2) -> inst -> ELM (sol1,sol2)" $ do
    context "aocTasks' :: (TestInstance -> ELM TestSolution1) (TestInstance -> ELM TestSolution2) -> TestInstance ELM (TestSolution1, TestSolution2)" $ do
      it "runELM . aocTasks' testSolve1Success testSolve2Success Inst 0 ->> Right (Inst 0, _)" $
        (runELM . aocTasks' testSolve1Success testSolve2Success) (Inst 0) `shouldBe` (Right ((Sol1 1, Sol2 2), map Text.pack ["Success in \"solve1\".", "Success in \"solve2\"."]))
      it "runELM . aocTasks' testSolve1Success testSolve2Failure Inst 0 ->> Left [\"Error in \"solve1\".\"]" $
        (runELM . aocTasks' testSolve1Success testSolve2Failure) (Inst 0) `shouldBe` Left (map Text.pack ["Error in \"solve2\"."])
      it "runELM . aocTasks' testSolve1Failure testSolve2Success Inst 0 ->> Left [\"Error in \"solve2\".\"]" $
        (runELM . aocTasks' testSolve1Failure testSolve2Success) (Inst 0) `shouldBe` Left (map Text.pack ["Error in \"solve1\"."])
      it "runELM . aocTasks' testSolve1Failure testSolve2Failure Inst 0 ->> Left [\"Error in \"solve1\".\"]" $
        (runELM . aocTasks' testSolve1Failure testSolve2Failure) (Inst 0) `shouldBe` (Left [Text.pack "Error in \"solve1\"."])
