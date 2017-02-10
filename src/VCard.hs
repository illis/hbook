{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module VCard (
  VCard(..)
  , ContentLine (..)
  , Param (..)
  , readVCard
  , searchValues
  , findFields

#ifdef TEST
  , readContentLine
  , semi
  , colon
  , readLine
  , readBlock
  , scoreCard

#endif 

) where

import Control.Applicative (empty)
import Control.Monad (void)
import qualified Data.ByteString.Char8 as B (pack, unpack)
import qualified Data.Text.Lazy as TL
import Codec.Binary.QuotedPrintable (decode)
import Data.Function ( on )
import Data.List ( delete, reverse, sortBy )
import Data.Maybe ( catMaybes )
import Data.Monoid ( (<>) )
import Text.Fuzzy as F
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.Text.Lazy
import qualified Text.Megaparsec.Lexer as L

default (TL.Text)

type Param = TL.Text

data ContentLine = ContentLine {
  name :: TL.Text,
  param :: [Param],
  value :: TL.Text
} deriving (Show,Eq)

data VCard = VCard {
  lines :: [ContentLine]
} deriving (Show,Eq)


-- depreciated
readContentLine :: TL.Text -> ContentLine
readContentLine l =
  let a = TL.splitOn ":" l
   in ContentLine { name = head a, param = [], value = a !! 1 }

sc :: Parser ()
sc = L.space (void spaceChar) empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

semi :: Parser String
semi = symbol ";"

colon :: Parser String
colon = symbol ":"

beginvcard :: Parser String
beginvcard = symbol "BEGIN:VCARD"

endvcard :: Parser String
endvcard = symbol "END:VCARD"

readLine :: Parser ContentLine
readLine = do
  p <- manyTill anyChar colon
  let a = TL.splitOn ";" $ TL.pack p
  let params = tail a 
  v <- manyTill anyChar eol
  return ContentLine { name = head a, param = deletedExtraParams params, value = decodedValue v params }
  where 
    encodingQP = "ENCODING=QUOTED-PRINTABLE"
    charsetUTF8 = "CHARSET=UTF-8"
    deletedExtraParams params = delete charsetUTF8 $ delete encodingQP params
    decodedValue val params = 
      if encodingQP `elem` params then
        case decode $ B.pack val of
          Left _ ->  TL.pack val
          Right x -> TL.pack $ B.unpack x
      else TL.pack val

readBlock :: Parser [ContentLine]
readBlock =
  beginvcard >> manyTill readLine endvcard

readVCard' :: Parser VCard
readVCard' = do
  b <- readBlock
  return VCard { VCard.lines = b }

readVCard :: Parser [VCard]
readVCard = some readVCard'

scoreCard :: TL.Text -> VCard -> Maybe (VCard, Int)
scoreCard s c =
  case length results of
    0 -> Nothing
    _ -> Just (c, score $ head results)
  where results = F.filter s (VCard.lines c) "" "" value False 

searchValues' :: [(VCard, Int)] -> Int -> [(VCard, Int)]
searchValues' [] _ = []
searchValues' (x : xs)  i
  | snd x >= i  = x : searchValues' xs i
  | otherwise = searchValues' xs i

searchValues :: TL.Text -> Int -> [VCard] -> [VCard]
searchValues s i c = reverse $ fst <$> sortBy comparitor minScored
  where 
    scored = catMaybes $ scoreCard s <$> c
    minScored = searchValues' scored i
    comparitor = compare `on` snd

findFields :: VCard -> TL.Text -> [ContentLine]
findFields c = it (VCard.lines c)
  where 
    it [] s = []
    it (x:xs) s 
      | name x == s = x : it xs s
      | otherwise  = it xs s
  
