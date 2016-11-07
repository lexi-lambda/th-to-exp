{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.TH.ToExpSpec (spec) where

import Test.Hspec

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.ToExp

spec :: Spec
spec = describe "toExp" $ do
  describe "integral literals" $ do
    it "converts Integers" $
      toExp (1 :: Integer) `shouldBe` SigE (LitE $ IntegerL 1) (ConT ''Integer)
    it "converts Ints" $
      toExp (1 :: Int) `shouldBe` SigE (LitE $ IntegerL 1) (ConT ''Int)

  it "converts Chars to character literals" $
    toExp 'a' `shouldBe` LitE (CharL 'a')

  it "converts lists to list literals" $
    toExp ['a', 'b', 'c'] `shouldBe`
      ListE [LitE (CharL 'a'), LitE (CharL 'b'), LitE (CharL 'c')]

  describe "Generic types" $ do
    it "converts ()" $
      toExp () `shouldBe` ConE '()
    it "converts Bool" $ do
      toExp True `shouldBe` ConE 'True
      toExp False `shouldBe` ConE 'False
    it "converts (Maybe a)" $ do
      toExp (Nothing :: Maybe Bool) `shouldBe` ConE 'Nothing
      toExp (Just True) `shouldBe` AppE (ConE 'Just) (ConE 'True)
    it "converts (Either a b)" $ do
      toExp (Left False :: Either Bool Bool) `shouldBe` AppE (ConE 'Left) (ConE 'False)
      toExp (Right True :: Either Bool Bool) `shouldBe` AppE (ConE 'Right) (ConE 'True)

  it "can persist the result of ‘reify’" $ do
    $(toExp <$> reify ''()) `shouldBe`
      TyConI (DataD [] ''() [] Nothing [NormalC '() []] [])
