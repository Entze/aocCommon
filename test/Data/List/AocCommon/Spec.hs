module Data.List.AocCommon.Spec where

import Test.Hspec
import Test.Hspec.LeanCheck as LC
import Test.Hspec.QuickCheck
import Test.QuickCheck as QC hiding ((===))
import Test.LeanCheck.Utils.Operators
import Safe

import Data.List

import CommonSpec
import AocCommon


test :: IO ()
test = hspec $ do
  describe "iterateUntilFixpoint :: Eq a => (a -> a) -> a -> [a]" $ do
    context "iterateUntilFixpoint id :: Int -> [Int]" $ do
      it "should be stable: LeanCheck" $
        LC.propertyFor 100000 $ isStable ((iterateUntilFixpoint id) :: Int -> [Int])
      modifyMaxSuccess (const 1000) $ it "should be stable: QuickCheck" $
        QC.property $ isStable ((iterateUntilFixpoint id) :: Int -> [Int])
  describe "iterateUntilFixpoint' :: Eq a => (a -> a) -> a -> [a]" $ do
    context "iterateUntilFixpoint' id :: Int -> [Int]" $ do
      it "should be stable: LeanCheck" $
        LC.propertyFor 100000 $ isStable ((iterateUntilFixpoint' id) :: Int -> [Int])
      modifyMaxSuccess (const 1000) $ it "should be stable: QuickCheck" $
        QC.property $ isStable((iterateUntilFixpoint' id) :: Int -> [Int])
  describe "takeUntilEqual :: Eq a => [a] -> [a]" $ do
    context "takeUntilEqual :: [Int] -> [Int]" $ do
      it "should be idempotent: LeanCheck" $
        LC.propertyFor 100000 $ isIdempotent (takeUntilEqual :: [Int] -> [Int])
      modifyMaxSuccess (const 1000) $ it "should be idempotent: QuickCheck" $
        QC.property $ isIdempotent (takeUntilEqual :: [Int] -> [Int])
    context "takeUntilEqual :: [Bool] -> [Bool]" $ do
      it "should be idempotent: LeanCheck" $
        LC.propertyFor 100000 $ isIdempotent (takeUntilEqual :: [Bool] -> [Bool])
      modifyMaxSuccess (const 1000) $ it "should be idempotent: QuickCheck" $
        QC.property $ isIdempotent (takeUntilEqual :: [Bool] -> [Bool])
  describe "gnomeSortBy :: Ord a => (a -> a -> Ordering) -> [a] -> [a]" $ do
    context "gnomeSortBy compare -> [Int] -> [Int]" $ do
      it "gnomeSortBy compare [1,0] ->> [0,1]" $
        gnomeSortBy compare [1,0] `shouldBe` ([0,1] :: [Int])
      it "gnomeSortBy compare [1,0,0] ->> [0,0,1]" $
        gnomeSortBy compare [1,0,0] `shouldBe` ([0,0,1] :: [Int])
      it "gnomeSortBy compare [1,1,0,0] ->> [0,0,1,1]" $
        gnomeSortBy compare [1,1,0,0] `shouldBe` ([0,0,1,1] :: [Int])
      it "should equal sort: LeanCheck" $
        LC.propertyFor 100000 $ (gnomeSortBy compare) === (sort :: [Int] -> [Int])
      modifyMaxSuccess (const 1000) $ it "should equal sort: QuickCheck" $
        QC.property $ (gnomeSortBy compare) === (sort :: [Int] -> [Int])
