module Data.String.AocCommon.Spec where

import AocCommon
import Test.Hspec
import Test.Hspec.LeanCheck as LC
import Test.LeanCheck.Utils.Operators
import Test.Hspec.QuickCheck
import Test.QuickCheck as QC hiding ((===))

test :: IO ()
test = hspec $ do
  describe "joinStringsBy :: Char -> [String] -> String" $ do
    it "joinStringsBy ',' (map show [1,2,3,4,5]) ->> \"1,2,3,4,5\"" $
      joinStringsBy ',' (map show ([1,2,3,4,5] :: [Int])) `shouldBe` "1,2,3,4,5"
    it "joinStringsBy ',' [] ->> \"\"" $
      joinStringsBy ',' [] `shouldBe` ""
    context "Show a => joinStringsBy ',' . ((map show) :: [a] -> [String])" $ do
      it "joinStringsBy ',' . (map show :: [Int] -> [String])) === drop 1 . init . show: LeanCheck" $
        LC.propertyFor 100000 $ (joinStringsBy ',' . ((map show) :: [Int] -> [String])) === (drop 1 . init . (show :: [Int] -> String))
      modifyMaxSuccess (const 1000) $ it "joinStringsBy ',' . (map show :: [Int] -> [String])) === drop 1 . init . show: QuickCheck" $
        QC.property $ (joinStringsBy ',' . ((map show) :: [Int] -> [String])) === (drop 1 . init . (show :: [Int] -> String))
      it "joinStringsBy ',' . (map show :: [String] -> [String])) === drop 1 . init . show: LeanCheck" $
        LC.propertyFor 100000 $ (joinStringsBy ',' . ((map show) :: [String] -> [String])) === (drop 1 . init . (show :: [String] -> String))
      modifyMaxSuccess (const 1000) $ it "joinStringsBy ',' . (map show :: [String] -> [String])) === drop 1 . init . show: QuickCheck" $
        QC.property $ (joinStringsBy ',' . ((map show) :: [String] -> [String])) === (drop 1 . init . (show :: [String] -> String))
    context "joinStringsBy ' ' " $ do
      it "joinStringsBy ' ' === unwords: LeanCheck" $
        LC.propertyFor 100000 $ (joinStringsBy ' ') === unwords
      modifyMaxSuccess (const 1000) $ it "joinStringsBy ' ' === unwords: QuickCheck" $
        QC.property $ (joinStringsBy ' ') === unwords
