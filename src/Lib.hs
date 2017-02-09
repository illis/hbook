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
import Text.Megaparsec
import Prelude hiding (putStrLn)

default (TL.Text)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

formatName :: VCard -> TL.Text
formatName c 
  | not (null fnFields) = value $ head fnFields
  | not (null nFields) = TL.pack $ printf "%s, %s" (head nSplits) (nSplits !! 1)
  | not (null nickNameFields) = value $ head nickNameFields
  | otherwise = ""
  where 
    fnFields = findFields c "FN"
    nFields = findFields c "N"
    nSplits = TL.splitOn ";" . value $ head nFields
    nickNameFields = findFields c "NICKNAME"

formatParamsForMutt :: [Param] -> TL.Text
formatParamsForMutt =  TL.intercalate ", " 

-- mutt format: <email address> <tab> <long name> <tab> <other info> <newline>
formatForMutt :: VCard -> TL.Text
formatForMutt c = foldr (\x -> TL.append (TL.pack $ printf "%s\t%s\t%s\n" (value x) name (paramStr x))) "" emailFields
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

