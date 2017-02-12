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
  , qpMultiEol
  , readLastLine
  , readMultiLineQP
  , decodeQP

#endif 

) where

import Control.Applicative (empty)
import Control.Monad (join, void)
import qualified Data.ByteString.Char8 as B (pack, unpack)
import qualified Data.Text.Lazy as TL
import Codec.Binary.QuotedPrintable (decode)
import Data.Function ( on )
import Data.List ( delete, reverse, sortBy, (++) )
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

qpMultiEol :: Parser String
qpMultiEol = do
  c <- string "="
  e <- eol
  return (c ++ e)

beginvcard :: Parser String
beginvcard = symbol "BEGIN:VCARD"

endvcard :: Parser String
endvcard = symbol "END:VCARD"

encodingQP = "ENCODING=QUOTED-PRINTABLE"
charsetUTF8 = "CHARSET=UTF-8"

readLastLine :: Parser String
readLastLine = manyTill printChar eol

readMultiLineQP :: Parser String
readMultiLineQP = do
  str <- many (try (someTill printChar (try qpMultiEol)))
  end <- readLastLine
  return (concat str ++ end)

decodeQP :: TL.Text -> TL.Text
decodeQP s = TL.pack $ decodeQP' $ TL.unpack s

decodeQP' :: String -> String
decodeQP' val = 
  case decode $ B.pack val of
    Left _ -> val
    Right x ->  B.unpack x

isQPEncoded :: [Param] -> Bool
isQPEncoded = elem encodingQP
   
readLine :: Parser ContentLine
readLine = do
  p <- manyTill anyChar colon
  let a = TL.splitOn ";" $ TL.pack p
  let params = tail a 
  if isQPEncoded params then do
    qp <- readMultiLineQP
    return ContentLine { name = head a, param = deletedExtraParams params, value = decodeQP $ TL.pack qp }
  else do
    s <- readLastLine
    return ContentLine { name = head a, param = deletedExtraParams params, value = TL.pack s }
  where 
    deletedExtraParams params = delete charsetUTF8 $ delete encodingQP params

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
  
