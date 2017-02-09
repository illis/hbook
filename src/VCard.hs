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
import qualified Data.Text.Lazy as TL
import Data.Function ( on )
import Data.List ( reverse, sortBy )
import Data.Maybe ( catMaybes )
import Data.Monoid ( (<>) )
import Text.Fuzzy as F
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.Text.Lazy
import qualified Text.Megaparsec.Lexer as L

default (TL.Text)

data Param = Param TL.Text
  deriving (Eq, Show)

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
  v <- manyTill anyChar eol
  return ContentLine { name = head a, param = Param <$> drop 1 a, value = TL.pack v }

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
  
