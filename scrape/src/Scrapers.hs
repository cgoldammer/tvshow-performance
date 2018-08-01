{-# LANGUAGE OverloadedStrings, FlexibleContexts, ScopedTypeVariables #-}

module Scrapers where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeUtf8With, decodeUtf8)
import Data.ByteString.Lazy as BSL (readFile, ByteString, toStrict)

import Text.Read (readMaybe)
import Data.Maybe (catMaybes, isJust, listToMaybe, fromJust)
import Control.Monad (join)
import Data.Map (Map, fromList, assocs, toAscList, findWithDefault, lookup)
import Debug.Trace
import Network.Curl (curlGetString)
import Data.Validation (Validation(Failure, Success), _Failure, _Success)

import System.IO.Unsafe (unsafePerformIO)

import Common (ParseSeason(..), ScrapeData(..), DownloadData(..), Validated, HtmlTable, FullData(..), Episode, OutcomesRow(..), getTable)
import HellsKitchen (scrapeDataHK)
import TopChef (scrapeDataTC)


getFile :: String -> IO TL.Text
getFile name = fmap decodeUtf8 $ BSL.readFile $ name

getData :: String -> Int -> IO TL.Text
getData name num = getFile $ "../data/downloaded/" ++ name ++ "_" ++ show num ++ ".html"

season = 1
textHK = unsafePerformIO $ getData "HellsKitchen" season
textTC = unsafePerformIO $ getData "TopChef" season
-- getter = fromJust $ (seasonGetter $ parseSeason scrapeDataHK) season
-- table = fromJust $ getter text
tableTC = fromJust $ getTable "wikitable" textTC
tableHK = fromJust $ getTable "wikitable" textHK
parsedHK = seasonParser (parseSeason scrapeDataHK) season tableHK
parsedTC = seasonParser (parseSeason scrapeDataTC) season tableTC

scrapeData = [scrapeDataTC]

summ (Just (Success _)) = True
summ _ = False

parseOne :: String -> ParseSeason -> Int -> IO ()
parseOne cleanName parseSeason season = do
  text <- getData cleanName season
  let validated = parseOne' cleanName parseSeason season text
  print $ cleanName ++ ", Season " ++ show season ++ " | " ++ show (summ validated)
  maybe (return ()) (exportCSV cleanName season) validated

parseOne' :: String -> ParseSeason -> Int -> TL.Text -> Maybe Validated
parseOne' cleanName (ParseSeason sGetter sParser) season text = fmap (sParser season) maybeTable
  where maybeTable = maybe Nothing (\getter -> getter text) (sGetter season) :: Maybe HtmlTable

toCSVValidated :: Validated -> Maybe T.Text
toCSVValidated (Failure _) = Nothing
toCSVValidated (Success (FullData episodes outcomes)) = Just $ toCSVData episodes outcomes

padValues :: Int -> a -> [a] -> [a]
padValues num def vals 
  | num <= length vals = vals
  | otherwise = vals ++ take (num - length vals) (repeat def)

toCSVData :: [Episode] -> [OutcomesRow] -> T.Text
toCSVData episodes outcomes = traceShow expandedOutcomes $ T.intercalate newLine $ fmap commaFold (withName : expandedOutcomes)
  where comma = T.pack ","
        withName = (T.pack "participant") : episodes
        commaFold = T.intercalate comma
        expandOutcome (OutcomesRow outcomesName outcomesVals) = outcomesName : (padValues (length episodes) "" outcomesVals)
        expandedOutcomes = (fmap expandOutcome outcomes) :: [[T.Text]]
        newLine = T.pack "\n"

export fileName (Just c) = writeFile fileName $ T.unpack c
export fileName Nothing = return ()

exportCSV :: String -> Int -> Validated -> IO ()
exportCSV showName episode validated = do
  let fileName = "../data/" ++ showName ++ "/season" ++ show episode ++ ".csv"
  let csv = toCSVValidated validated
  export fileName csv
  return ()

-- Parse all episodes and export them
parseAll :: ScrapeData -> IO ()
parseAll (ScrapeData (DownloadData url cleanName) seasonParser seasons) = do
  mapM_ (\season -> parseOne cleanName seasonParser season) seasons

-- Download the html to /data
downloadAll :: ScrapeData -> IO ()
downloadAll (ScrapeData (DownloadData url cleanName) _ seasons) = mapM_ (downloadOne url cleanName) seasons

doAll :: ScrapeData -> IO ()
doAll dat = downloadAll dat >> parseAll dat

getUrl :: String -> Int -> String
getUrl name num = "https://en.wikipedia.org/" ++ name ++ show num ++ ")"

downloadOne :: String -> String -> Int -> IO ()
downloadOne name cleanedName num = do
  (_, html) <- curlGetString (getUrl name num) []
  writeFile ("../data/downloaded/" ++ cleanedName ++ "_" ++ show num ++ ".html") html
  return ()


