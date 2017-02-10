{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module VCardSpec (spec) where

import VCard
import Test.Hspec
import Test.Hspec.Megaparsec
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Instances
import qualified Data.Text.Lazy as TL
import Data.Monoid ( (<>) )
import Control.Monad ( replicateM )
import Text.Megaparsec ( parse )

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
  describe "readContentLine" $
    it "reads a basic name & value line" $ property $
      forAll ((,) <$> upperText <*> upperText) $ \(x, y) -> readContentLine(x <> ":" <> y) `shouldBe` ContentLine { name = x, value = y, param = [] }

  describe "semi" $
    it "picks up a semi colon" $ 
      parse semi "" ";" `shouldParse` ";"

  describe "colon" $
    it "picks up a colon" $ 
      parse colon "" ":" `shouldParse` ":"

  describe "readLine" $ do
    it "reads a basic line" $
      parse readLine "" "TEL:123\n" `shouldParse` ContentLine { name = "TEL", param = [], value = "123" }
    it "reads a param" $
      parse readLine "" "TEL;MOBILE:123\n" `shouldParse` ContentLine { name = "TEL", param = ["MOBILE"], value = "123" }
    it "reads multiple params" $
      parse readLine "" "TEL;MOBILE;WORK:456\n" `shouldParse` ContentLine { name = "TEL", param = ["MOBILE", "WORK"], value = "456" }
    it "reads a quoted-printable encoded line" $
      parse readLine "" "N;CHARSET=UTF-8;ENCODING=QUOTED-PRINTABLE:this=20is=20a=20test;sfsadf;;;\n" `shouldParse` ContentLine { name = "N", param = [], value = "this is a test;sfsadf;;;" }
    
  describe "readBlock" $
    it "reads a basic block line" $
      parse readBlock "" "BEGIN:VCARD\nTEL:123\nEND:VCARD\n" `shouldParse` [ContentLine { name = "TEL", value = "123", param = [] }]

  describe "readVCard" $ do
    it "reads a single vcard" $
      parse readVCard "" "BEGIN:VCARD\nTEL:123\nEND:VCARD\n" `shouldParse` [VCard { VCard.lines = [ContentLine { name = "TEL", value = "123", param = [] }] }]

    it "reads concatinated vcards" $
      parse readVCard "" "BEGIN:VCARD\nTEL:123\nEND:VCARD\nBEGIN:VCARD\nEMAIL:noone@nowhere.com\nEND:VCARD\n" `shouldParse` [VCard { VCard.lines = [ContentLine { name = "TEL", value = "123", param = [] }] }, VCard { VCard.lines = [ContentLine { name = "EMAIL", value = "noone@nowhere.com", param = [] }] }]

  let card123 = VCard { VCard.lines = [ContentLine { name = "TEL", value = "123", param = [] }] }
  let cardm3 = VCard { VCard.lines = [ContentLine { name = "EMAIL", value = "no11e@nowhere.com", param = [] }] }
  let cardm5 = VCard { VCard.lines = [ContentLine { name = "EMAIL", value = "noone@nowhere.com", param = [] }] }
  let clSuEmail = ContentLine { name = "EMAIL", value = "chris.su@nowhere.com", param = ["HOME"] }
  let clSuEmail2 = ContentLine { name = "EMAIL", value = "chris.su@somewhere.com", param = ["WORK"] }

  let cardSu = VCard { VCard.lines = [
      ContentLine { name = "TEL", value = "123", param = [] },
      clSuEmail, clSuEmail2,
      ContentLine { name = "N", value = "Su;Chris;;;", param = [] } ]}

  describe "scoreCard" $ do
    it "returns a score for a matched card" $
      scoreCard "one" cardm5 `shouldBe` Just (cardm5, 5)

    it "returns a score for a matched card (2)" $
      scoreCard "one" cardm3 `shouldBe` Just (cardm3, 3)

    it "returns Nothing for a un-matched card" $
      scoreCard "123" cardm5 `shouldBe` Nothing

  describe "searchValues" $ do
    it "does a basic search on values and returns back a list of matching vcards" $
      searchValues "one" 0 [card123, cardm5] `shouldBe` [cardm5]

    it "returns only matches with a min score" $
      searchValues "one" 4 [cardm5, cardm3] `shouldBe` [cardm5]

    it "returns ordered decending by score" $ 
      searchValues "one" 0 [cardm3, cardm5, cardm3] `shouldBe` [cardm5, cardm3, cardm3]

  describe "findFields" $ do
    it "returns the relevant ContentLines" $
      findFields cardSu "EMAIL" `shouldBe` [clSuEmail, clSuEmail2]

    it "returns an empty array if no match is found" $ 
      findFields cardSu "NICKNAME" `shouldBe` []

      
