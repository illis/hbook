{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module LibSpec (spec) where

import Lib
import VCard
import Test.Hspec
import Test.Hspec.Megaparsec
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Instances
import qualified Data.Text.Lazy as TL
import Data.Monoid ( (<>) )
import Control.Monad ( replicateM )

default (TL.Text)

newtype UpperChar = UpperChar Char deriving (Eq, Show)

upperText :: Gen TL.Text
upperText = do
  l <- choose(2,50)
  TL.pack <$> replicateM l capitals
     where capitals = elements ['A'..'Z']

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
  let clSuEmail = ContentLine { name = "EMAIL", value = "chris.su@nowhere.com", param = ["HOME"] }
  let clSuEmail2 = ContentLine { name = "EMAIL", value = "chris.su@somewhere.com", param = ["WORK"] }

  let cardSuNoName = VCard { VCard.lines = [
      ContentLine { name = "TEL", value = "123", param = [] },
      clSuEmail, clSuEmail2  ]}

  let cardSu = VCard { VCard.lines = [
      ContentLine { name = "TEL", value = "123", param = [] },
      clSuEmail, clSuEmail2,
      ContentLine { name = "N", value = "Su;Chris;;;", param = [] } ]}

  let cardSuFN = VCard { VCard.lines = [
      ContentLine { name = "TEL", value = "123", param = [] },
      clSuEmail, clSuEmail2,
      ContentLine { name = "N", value = "Su;Chris;;;", param = [] },
      ContentLine { name = "FN", value = "My FN", param = [] } ]}

  let cardSuNN = VCard { VCard.lines = [
      ContentLine { name = "TEL", value = "123", param = [] },
      clSuEmail, clSuEmail2,
      ContentLine { name = "NICKNAME", value = "My NICKNAME", param = [] } ]}

  describe "formatName" $ do
    it "uses the FN field first" $
      formatName cardSuFN `shouldBe` "My FN"

    it "uses the N field second" $ 
      formatName cardSu `shouldBe` "Su, Chris"

    it "uses the NICKNAME third" $
      formatName cardSuNN `shouldBe` "My NICKNAME"

    it "uses a blank string if nothing else is available" $
      formatName cardSuNoName `shouldBe` ""

  describe "formatForMutt" $
  -- mutt format: <email address> <tab> <long name> <tab> <other info> <newline><Paste>
    it "formats a card to mutt format" $ 
      formatForMutt cardSu `shouldBe` "chris.su@nowhere.com\tSu, Chris\tHOME\nchris.su@somewhere.com\tSu, Chris\tWORK\n"

  describe "formatParamsForMutt" $ do
    it "formats a single property field" $ 
      formatParamsForMutt ["WORK"] `shouldBe` "WORK"

    it "formats a multiple property fields" $ 
      formatParamsForMutt ["WORK", "SECOND"] `shouldBe` "WORK, SECOND"

