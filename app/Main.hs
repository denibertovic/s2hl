-- {-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Semigroup      ((<>))
import qualified Data.Text           as T
import           Options.Applicative
import           S2HL.Lib
import           S2HL.Options

main :: IO ()
main = execParser opts >>= entrypoint
  where
    opts = info (helper <*> versionOpt <*> statement2HledgerArgs)
      ( fullDesc
     <> progDesc "Converts from Erste CSV and HTML statements to\
     \ HLedger journal format."
     <> header "s2hl - A converter from Erste CSV/HTML\
     \ statemets to Hledger journal format.\
     \ Requires the directory structure  SOMEPATH/statements/{CURRENCY}/*.{csv,html}" )


-- import           Text.Parsec

-- field = many1 (noneOf "\t")
-- quoted_field = between (char '"') (char '"') (many (noneOf "\""))
-- line = (quoted_field <|> field) `sepBy1` (char '\t')
-- tsvfile = many line

-- main = interact $ show . runParser tsvfile "" ""

