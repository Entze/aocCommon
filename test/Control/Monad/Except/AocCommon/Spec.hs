module Control.Monad.Except.AocCommon.Spec where

import Test.Hspec
--import Test.Hspec.LeanCheck as LC
--import Test.Hspec.QuickCheck
--import Test.QuickCheck as QC hiding ((===))
--import Safe

import AocCommon


test :: IO ()
test = hspec $ do
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
