{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib (
  someFunc

#ifdef TEST
  , formatParamsForMutt
  , formatForMutt
  , formatName

#endif

) where

import VCard
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.IO (putStrLn, hGetContents)
import System.IO (IOMode(..), withFile) 
import Text.Printf
import Text.Megaparsec (parse)
import Prelude hiding (putStrLn)
import Options.Applicative

default (TL.Text)

data Opts = Opts
  { query :: String
  , minscore :: Int
  , filename :: FilePath
  }

parserOpts :: Parser Opts
parserOpts = Opts
  <$> strOption
     ( metavar "Query"
    <> long "query"
    <> short 'q'
    <> help "Query" )
  <*> option auto
     ( metavar "MinScore"
    <> short 'm'
    <> long "minscore"
    <> help "Min Score (0 - 5)" )
  <*> strOption
     ( metavar "Filename"
    <> long "file"
    <> short 'f'
    <> help "File containing vcards" )

muttQueryParserOpts :: Opts -> IO ()
muttQueryParserOpts (Opts query minscore fn) = muttQuery (TL.pack query) minscore fn

someFunc :: IO ()
someFunc = muttQueryParserOpts =<< execParser opts
  where
    opts = info (parserOpts <**> helper)
      (  fullDesc
      <> progDesc "Mutt VCF lookup"
      <> header "hbook - VCF address book lookup tool for mutt "
      )

formatName :: VCard -> TL.Text
formatName c 
  | not (null fnFields) = VCard.value $ head fnFields
  | not (null nFields) = TL.pack $ printf "%s, %s" (head nSplits) (nSplits !! 1)
  | not (null nickNameFields) = VCard.value $ head nickNameFields
  | otherwise = ""
  where 
    fnFields = findFields c "FN"
    nFields = findFields c "N"
    nSplits = TL.splitOn ";" . VCard.value $ head nFields
    nickNameFields = findFields c "NICKNAME"

formatParamsForMutt :: [Param] -> TL.Text
formatParamsForMutt =  TL.intercalate ", " 

-- mutt format: <email address> <tab> <long name> <tab> <other info> <newline>
formatForMutt :: VCard -> TL.Text
formatForMutt c = foldr (\x -> TL.append (TL.pack $ printf "%s\t%s\t%s\n" (VCard.value x) name (paramStr x))) "" emailFields
  where 
    name = formatName c
    emailFields = findFields c "EMAIL"
    paramStr x = formatParamsForMutt $ param x

muttQuery :: TL.Text -> Int -> FilePath -> IO ()
muttQuery query minScore fileName = 
  withFile fileName ReadMode (\handle -> do
    vcf <- hGetContents handle
    case parse readVCard "" vcf of
      Left x -> putStrLn "Error"
      Right x -> putStrLn $ TL.intercalate "" (formatForMutt <$> searchValues query minScore x))

