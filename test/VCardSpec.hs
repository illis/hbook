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
      forAll ((,) <$> upperText <*> upperText) $ \(x, y) -> readContentLine(x <> ":" <> y) `shouldBe` ContentLine { name = x, value = y, prop = [] }

  describe "semi" $
    it "picks up a semi colon" $ 
      parse semi "" ";" `shouldParse` ";"

  describe "colon" $
    it "picks up a colon" $ 
      parse colon "" ":" `shouldParse` ":"

  describe "readLine" $
    it "reads a basic line" $
      parse readLine "" "TEL:123\n" `shouldParse` ContentLine { name = "TEL", prop = [], value = "123" }

  describe "readBlock" $
    it "reads a basic block line" $
      parse readBlock "" "BEGIN:VCARD\nTEL:123\nEND:VCARD\n" `shouldParse` [ContentLine { name = "TEL", value = "123", prop = [] }]

  describe "readVCard" $  do
    it "reads a single vcard" $
      parse readVCard "" "BEGIN:VCARD\nTEL:123\nEND:VCARD\n" `shouldParse` [VCard { VCard.lines = [ContentLine { name = "TEL", value = "123", prop = [] }] }]

    it "reads a concatinated vcards" $
      parse readVCard "" "BEGIN:VCARD\nTEL:123\nEND:VCARD\nBEGIN:VCARD\nEMAIL:noone@nowhere.com\nEND:VCARD\n" `shouldParse` [VCard { VCard.lines = [ContentLine { name = "TEL", value = "123", prop = [] }] }, VCard { VCard.lines = [ContentLine { name = "EMAIL", value = "noone@nowhere.com", prop = [] }] }]
