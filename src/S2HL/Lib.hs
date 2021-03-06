{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module S2HL.Lib where

import           Control.Monad                       (forM, forM_, when, (=<<))
import           Control.Monad.Writer
import qualified Data.ByteString.Lazy                as B
import           Data.Char                           (ord)
import           Data.Csv
import           Data.List                           (elem, filter, sort)
import           Data.Map                            (Map, fromList, (!))
import qualified Data.Text.Lazy                      as T
import           Data.Text.Lazy.Encoding             as E
import qualified Data.Text.Lazy.IO                   as TIO
import           Data.Time                           (Day)
import           Data.Time.Format                    (defaultTimeLocale,
                                                      formatTime,
                                                      parseTimeOrError)
import qualified Data.Vector                         as V
import           System.Directory                    (doesFileExist,
                                                      listDirectory,
                                                      makeAbsolute, removeFile)
import           System.FilePath                     ((</>), takeExtension)
import           System.IO                           (IOMode (..), hGetContents,
                                                      hGetLine, hSetEncoding,
                                                      openFile, utf16)
import           Text.HTML.Scalpel
import           Text.HTML.TagSoup
import           Text.StringTemplate
import           Text.StringTemplate.GenericStandard

import           S2HL.Options
import           S2HL.Types


hledgerOutputFileName :: String
hledgerOutputFileName = "hledger.journal"

template :: StringTemplate String
template = newSTMP $ unlines [
            "$context.hlDate$ $context.hlDescription$"
          , "    expenses:$context.hlCurrency$         $context.hlExpenses$"
          , "    assets:$context.hlCurrency$           $context.hlAssets$\n\n"
          ]

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

generateHledgerText :: StringTemplate String -> HledgerContext -> T.Text
generateHledgerText template c =
            T.pack $ toString $ setAttribute "context" context template
        where context = c { hlExpenses = curPlusAmount $ hlExpenses c
                          , hlAssets = curPlusAmount $ hlAssets c
                          }
              curTxt = T.pack . show . hlCurrency $ c
              curPlusAmount a = case a of
                                 ""        -> ""
                                 otherwise -> (T.append a $ curTxt)

fixDateFormat :: T.Text -> T.Text
fixDateFormat d = T.pack . newDate $ d
    where
        oldFormat = "%-d.%-m.%Y." :: String
        newFormat = "%Y/%-m/%-d" :: String
        oldDate d = (parseTimeOrError True defaultTimeLocale oldFormat d) :: Day
        newDate d = (formatTime defaultTimeLocale newFormat (oldDate . T.unpack $ d)) :: String

csv2Hledger :: FilePath -> Currency -> IO [T.Text]
csv2Hledger f c = do
        contents <- readFileWithConversion f
        let res = (decodeByNameWith tabDelimited contents) :: Either String (Header, V.Vector ErsteCSV)
        case res of
            Left  err            -> error err
            Right (header, rows) ->  return $ V.toList $ V.map row2TextHelper rows
    where
        tabDelimited = defaultDecodeOptions {
               decDelimiter = fromIntegral (ord '\t')}
        row2TextHelper :: ErsteCSV -> T.Text
        row2TextHelper row =
                        let context = HledgerContext
                                { hlCurrency = c
                                , hlDate = (fixDateFormat . dateOfCurrency $ row)
                                , hlDescription = description row
                                , hlExpenses = T.strip . amountOut $ row
                                , hlAssets = T.strip . amountIn $ row
                                }
                        in generateHledgerText template context

html2Hledger :: FilePath -> Currency -> IO [T.Text]
html2Hledger f c = do
        src <- TIO.readFile f
        let contexts = scrapeStringLike src getStatements :: Maybe [HledgerContext]
        case contexts of
            Nothing -> error "failed to parse html"
            Just cs -> do
                forM cs $  \c -> do
                    return $ generateHledgerText template c
    where
        trStr = "tr"
        tdStr = "td"
        trItemsStr = "trItems"
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

processFile :: FilePath -> Currency -> IO [T.Text]
processFile f cur = do
  let csvLike = [".CSV", ".csv"]
  let htmlLike = [".html", ".HTML"]
  let ext = takeExtension f
  case ext of
    e | e `elem` csvLike  -> csv2Hledger f cur
      | e `elem` htmlLike -> html2Hledger f cur
    otherwhise -> error $ "Unsupported file extension: " <> ext

processLocalCurrency :: FilePath -> FilePath -> WriterT [FilePath] IO ()
processLocalCurrency sDir oFile = do
    fps <- liftIO $ listStatements sDir HRK
    tell fps
    forM_ fps $  \f -> do
        res <- liftIO $ processFile f HRK
        liftIO $ mapM_ (TIO.appendFile oFile) res

processForeignCurrencies :: FilePath -> FilePath -> [Currency] -> WriterT [FilePath] IO ()
processForeignCurrencies sDir oFile curs =  do
    forM_ curs $ \c -> do
        fps <- liftIO $ listStatements sDir c
        tell fps
        forM_ fps $ \f -> do
            res <- liftIO $ processFile f c
            liftIO $ mapM_ (TIO.appendFile oFile) res

entrypoint :: Statement2HledgerArgs -> IO ()
entrypoint (Statement2HledgerArgs sDir oDir curs d) = do
        let hledgerFilePath = (oDir </> hledgerOutputFileName)
        exists <- doesFileExist hledgerFilePath
        when exists (removeFile hledgerFilePath)
        lcfs <- execWriterT . when (elem HRK curs) $ processLocalCurrency sDir hledgerFilePath
        fcfs <- execWriterT . when (foreignCurs /= []) $ processForeignCurrencies sDir hledgerFilePath foreignCurs
        let processedFiles = lcfs ++ fcfs
        when (d) $ forM_ processedFiles $ \f -> do
            putStr "Processing file: "
            putStrLn f
    where
        foreignCurs = filter (\c -> c /= HRK) curs

