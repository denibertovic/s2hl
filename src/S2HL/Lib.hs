{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module S2HL.Lib where

import           Control.Monad           (forM, forM_, when, (=<<))
import qualified Data.ByteString.Lazy    as B
import           Data.Char               (ord)
import           Data.Csv
import           Data.List               (elem, filter, sort)
import           Data.Map                (Map, fromList, (!))
import qualified Data.Text.Lazy          as T
import           Data.Text.Lazy.Encoding as E
import qualified Data.Text.Lazy.IO       as TIO
import           Data.Time               (Day)
import           Data.Time.Format        (defaultTimeLocale, formatTime,
                                          parseTimeOrError)
import qualified Data.Vector             as V
import           System.Directory        (doesFileExist, listDirectory,
                                          makeAbsolute, removeFile)
import           System.FilePath         ((</>))
import           System.IO               (IOMode (..), hGetContents, hGetLine,
                                          hSetEncoding, openFile, utf16)
import           Text.Hastache
import           Text.Hastache.Context
import           Text.HTML.Scalpel
import           Text.HTML.TagSoup

import           S2HL.Options
import           S2HL.Types


hledgerOutputFileName :: String
hledgerOutputFileName = "hledger.journal"

template :: T.Text
template = "{{DATE}} {{DESCRIPTION}}\n\
\    expenses:{{CURRENCY}}         {{EXPENSES}}\n\
\    assets:{{CURRENCY}}           {{ASSETS}}\n\n"

listStatements :: FilePath -> Currency -> IO [FilePath]
listStatements d c = do
        fs <- listDirectory (d </> show c)
        return $ map (dir </>) $ sort fs
    where
        dir = d </> show c

readFileWithConversion :: FilePath -> IO B.ByteString
readFileWithConversion f = do
    handle <- openFile f ReadMode
    hSetEncoding handle utf16
    _ <- hGetLine handle
    contents <- B.hGetContents handle
    let contentsUtf8 = E.encodeUtf8 . E.decodeUtf16LE $ contents
    return contentsUtf8

generateHledgerText :: T.Text -> HledgerContext -> IO T.Text
generateHledgerText template c = do
            rendered <- hastacheStr defaultConfig (encodeStr . T.unpack $ template) (mkStrContext context)
            return rendered
        where context "DATE" = MuVariable (hlDate c)
              context "DESCRIPTION" = MuVariable (hlDescription c)
              context "CURRENCY" = MuVariable (curTxt)
              context "EXPENSES" = MuVariable (curPlusAmount $ hlExpenses c)
              context "ASSETS" = MuVariable (curPlusAmount $ hlAssets c)
              curTxt = T.pack . show . hlCurrency $ c
              curPlusAmount a = case a of
                                 "" -> ""
                                 otherwise -> (T.append a $ curTxt)

fixDateFormat :: T.Text -> T.Text
fixDateFormat d = T.pack . newDate $ d
    where
        oldFormat = "%-d.%-m.%Y." :: String
        newFormat = "%Y/%-m/%-d" :: String
        oldDate d = (parseTimeOrError True defaultTimeLocale oldFormat d) :: Day
        newDate d = (formatTime defaultTimeLocale newFormat (oldDate . T.unpack $ d)) :: String

csv2Hledger :: FilePath -> Currency -> IO (V.Vector T.Text)
csv2Hledger f c = do
        print f
        contents <- readFileWithConversion f
        -- let res = decodeWith tabDelimited HasHeader contents
        let res = (decodeByNameWith tabDelimited contents) :: Either String (Header, V.Vector ErsteCSV)
        case res of
            Left  err -> error err
            Right (header, rows) -> forM rows $ row2TextHelper
    where
        tabDelimited = defaultDecodeOptions {
               decDelimiter = fromIntegral (ord '\t')}
        row2TextHelper row = do
                        let context = HledgerContext
                                { hlCurrency = c
                                , hlDate = (fixDateFormat . dateOfCurrency $ row)
                                , hlDescription = description row
                                , hlExpenses = T.strip . amountOut $ row
                                , hlAssets = T.strip . amountIn $ row
                                }
                        hl <- generateHledgerText template context
                        return hl

html2Hledger :: FilePath -> Currency -> IO [T.Text]
html2Hledger f c = do
        print f
        src <- TIO.readFile f
        let contexts = scrapeStringLike src getStatements :: Maybe [HledgerContext]
        case contexts of
            Nothing -> error "failed to parse html"
            Just cs -> do
                forM cs $  \c -> do
                    hl <- generateHledgerText template c
                    return hl
    where
        trStr = "tr" :: String
        tdStr = "td" :: String
        trItemsStr = "trItems" :: String
        getStatements :: Scraper T.Text [HledgerContext]
        getStatements = chroots (trStr @: [hasClass trItemsStr]) tds

        tds :: Scraper T.Text HledgerContext
        tds = do
            res <- chroots tdStr td
            return $ HledgerContext
                     { hlDate = fixDateFormat . splitDate $ res !! 0
                     , hlCurrency = c
                     , hlDescription = res !! 2
                     , hlExpenses = T.strip $ res !! 4
                     , hlAssets = T.strip $ res !! 5
                     }

        td :: Scraper T.Text T.Text
        td = do
            text $ tdStr

        splitDate d = T.intercalate "." $ lastN 4 $ T.splitOn "." d
        lastN :: Int -> [a] -> [a]
        lastN n xs = drop (length xs - n) xs

processLocalCurrency :: FilePath -> FilePath -> IO ()
processLocalCurrency sDir oFile = do
    fps <- listStatements sDir HRK
    forM_ fps $  \f -> do
        res <- csv2Hledger f HRK
        V.mapM_ (TIO.appendFile oFile) res

processForeignCurrencies :: FilePath -> FilePath -> [Currency] -> IO ()
processForeignCurrencies sDir oFile curs =  do
    forM_ curs $ \c -> do
        fps <- listStatements sDir c
        forM_ fps $ \f -> do
            res <- html2Hledger f c
            mapM_ (TIO.appendFile oFile) res

entrypoint :: Statement2HledgerArgs -> IO ()
entrypoint (Statement2HledgerArgs sDir oDir curs d) = do
        let hledgerFilePath = (oDir </> hledgerOutputFileName)
        exists <- doesFileExist hledgerFilePath
        when exists (removeFile hledgerFilePath)
        when (elem HRK curs) $ processLocalCurrency sDir hledgerFilePath
        when (foreignCurs /= []) $ processForeignCurrencies sDir hledgerFilePath foreignCurs
        print "SUCCESS."
    where
        foreignCurs = filter (\c -> c /= HRK) curs

