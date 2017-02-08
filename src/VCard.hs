{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module VCard (
  VCard(..)

#ifdef TEST
  , ContentLine (..)
  , Param (..)
  , readContentLine
  , semi
  , colon
  , readLine
  , readBlock
  , readVCard

#endif 

) where

import Control.Applicative (empty)
import Control.Monad (void)
import qualified Data.Text.Lazy as TL
import Data.Monoid ( (<>) )
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.Text.Lazy
import qualified Text.Megaparsec.Lexer as L

default (TL.Text)

data Param = Param TL.Text
  deriving (Show, Eq)

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

