
import Test.Hspec
import Test.Hspec.LeanCheck as LC
import Test.Hspec.QuickCheck
import Test.QuickCheck as QC
import Safe


import AocCommon


main :: IO ()
main = hspec $ do
  describe "maybeToError :: (Monoid e, MonadError e m) => Maybe a -> m a" $ do
    context "maybeToError :: Maybe Int -> Either String Int" $ do
      it "maybeToError (Just a) ->> Right a" $
        ((maybeToError :: Maybe Int -> Either String Int) (Just 1)) `shouldBe` Right 1
      it "maybeToError Nothing ->> Left []" $
        ((maybeToError :: Maybe Int -> Either String Int) Nothing) `shouldBe` Left []
  describe "maybeToErrorWith :: MonadError e m => e -> Maybe a -> m a" $ do
    context "maybeToErrorWith :: String -> Maybe Int -> Either String Int" $ do
      it "maybeToErrorWith \"error\" (Just a) ->> Right a" $
        ((maybeToErrorWith :: String -> Maybe Int -> Either String Int) "error" (Just 1)) `shouldBe` Right 1
      it "maybeToErrorWith \"error\" Nothing ->> Left \"error\"" $
        ((maybeToErrorWith :: String -> Maybe Int -> Either String Int) "error" Nothing) `shouldBe` Left "error"
  describe "iterateUntilFixpoint :: Eq a => (a -> a) -> a -> [a]" $ do
    context "iterateUntilFixpoint id :: Int -> [Int]" $ do
      it "should be stable: LeanCheck" $
        LC.propertyFor 100000 $ propStable1 ((iterateUntilFixpoint id) :: Int -> [Int])
      modifyMaxSuccess (const 1000) $ it "should be stable: QuickCheck" $
        QC.property $ propStable1 ((iterateUntilFixpoint id) :: Int -> [Int])
  describe "iterateUntilFixpoint' :: Eq a => (a -> a) -> a -> [a]" $ do
    context "iterateUntilFixpoint' id :: Int -> [Int]" $ do
      it "should be stable: LeanCheck" $
        LC.propertyFor 100000 $ propStable1 ((iterateUntilFixpoint' id) :: Int -> [Int])
      modifyMaxSuccess (const 1000) $ it "should be stable: QuickCheck" $
        QC.property $ propStable1 ((iterateUntilFixpoint' id) :: Int -> [Int])
  describe "takeUntilEqual :: Eq a => [a] -> [a]" $ do
    context "takeUntilEqual :: [Int] -> [Int]" $ do
      it "should be idempotent: LeanCheck" $
        LC.propertyFor 100000 $ propIdempotent1 (takeUntilEqual :: [Int] -> [Int])
      modifyMaxSuccess (const 1000) $ it "should be idempotent: QuickCheck" $
        QC.property $ propIdempotent1 (takeUntilEqual :: [Int] -> [Int])
    context "takeUntilEqual :: [Bool] -> [Bool]" $ do
      it "should be idempotent: LeanCheck" $
        LC.propertyFor 100000 $ propIdempotent1 (takeUntilEqual :: [Bool] -> [Bool])
      modifyMaxSuccess (const 1000) $ it "should be idempotent: QuickCheck" $
        QC.property $ propIdempotent1 (takeUntilEqual :: [Bool] -> [Bool])


propIdempotent1 :: Eq a => (a -> a) -> a -> Bool
propIdempotent1 f a = a' == f a'
  where
    a' = f a

propStable1 :: Eq a => (a -> [a]) -> a -> Bool
propStable1 f a = end == (end >>= (lastMay . f))
  where
    end = lastMay a'
    a' = f a
