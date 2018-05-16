{-# LANGUAGE OverloadedStrings #-}

module S2HL.Options where

import           Data.Semigroup      ((<>))
import           Data.Version        (showVersion)
import           Options.Applicative
import           Paths_s2hl          (version)
import           S2HL.Types

data Statement2HledgerArgs = Statement2HledgerArgs
            { statemntsDir :: FilePath
            , outputDir    :: FilePath
            , currency     :: [Currency]
            , debug        :: Bool
            }


versionOpt = infoOption (showVersion version) (
               long "version"
               <> short 'v'
               <> help "Show version.")

statement2HledgerArgs :: Parser Statement2HledgerArgs
statement2HledgerArgs = Statement2HledgerArgs
    <$> strOption
        ( long "statements-dir"
        <> short 's'
        <> metavar "DIR"
        <> help "Absolute path to the directory hosting bank statement files." )
    <*> strOption
        ( long "output-dir"
        <> short 'o'
        <> metavar "DIR"
        <> help "Absolute path to directory where to write the hledger journal file" )
    <*> some ( option auto
               ( long "currency"
               <> short 'c'
               <> metavar "CUR"
               <> help "Currency on the statements: HRK, USD, EUR" ) )
    <*> switch
        ( long "debug"
        <> help "Print out results to stdout but don't actually create a ledger file" )

