{-# LANGUAGE OverloadedStrings, FlexibleContexts, ScopedTypeVariables #-}

module Scrapers where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeUtf8With, decodeUtf8)
import Data.ByteString.Lazy as BSL (readFile, ByteString, toStrict)

import Data.Tuple (swap)
import Text.Read (readMaybe)
import Data.Maybe (catMaybes, isJust, listToMaybe, fromJust)
import Control.Monad (join)
import Data.Map (Map, fromList, assocs, toAscList, findWithDefault, lookup)
import Debug.Trace
import Network.Curl (curlGetString)
import Data.Validation (Validation(Failure, Success), _Failure, _Success)
import Data.CSV (genCsvFile)
import System.Directory (createDirectoryIfMissing)

import System.IO.Unsafe (unsafePerformIO)

import Common (AllTableParser(..), Table(..), ScrapeData(..), DownloadData(..), Validated, HtmlTable, FullData(..), Episode, OutcomesRow(..), getTable, FullExportData(..), allTables, parseTable)
import ShowScrapers (scrapeDataHK, scrapeDataFNS, scrapeDataRP, scrapeDataTC, scrapeDataMC)

-- Summary

getFile :: String -> IO TL.Text
getFile name = fmap decodeUtf8 $ BSL.readFile $ name

getData :: String -> Int -> IO TL.Text
getData name num = getFile $ "../downloaded/" ++ name ++ "_" ++ show num ++ ".html"

season = 1
text = unsafePerformIO $ getData "TopChef" season
table = allTables text
parsed = fmap (parseTable ((seasonParser scrapeDataTC) season)) table

scrapeData = [scrapeDataHK, scrapeDataTC, scrapeDataFNS, scrapeDataRP, scrapeDataMC]

summ (PersonTable (Success _)) = "Persontable"
summ (OutcomeTable (Success _)) = "Outcometable"
summ x = show x

tableName :: Table a -> String
tableName (PersonTable _) = "participants"
tableName (OutcomeTable _) = "outcomes"

toExportData :: Table Validated -> (String, Validated)
toExportData (PersonTable val) = ("participants", val)
toExportData (OutcomeTable val) = ("outcomes", val)

summShort (PersonTable (Success _)) = "P"
summShort (OutcomeTable (Success _)) = "O"
summShort x = ""

summResults :: [Table Validated] -> String
summResults vals = if length short == 0 then "-" else concat short
  where short = fmap summShort vals

parseOne :: String -> (Int -> AllTableParser) -> Int -> IO String
parseOne cleanName sParser season = do
  text <- getData cleanName season
  let validated = parseOne' cleanName sParser season text
  print $ cleanName ++ ", Season " ++ show season ++ " | " ++ show (fmap summ validated)
  let exporter = uncurry $ exportCSV cleanName season
  let exportData = fmap toExportData validated
  mapM_ exporter exportData
  return $ summResults validated


parseOne' :: String -> (Int -> AllTableParser) -> Int -> TL.Text -> [Table Validated]
parseOne' cleanName sParser season text = fmap (parseTable (sParser season)) tables
  where tables = allTables text

toCSVValidated :: Validated -> Maybe T.Text
toCSVValidated (Failure _) = Nothing
toCSVValidated (Success (FullData episodes outcomes)) = Just $ toCSVData episodes outcomes

padValues :: Int -> a -> [a] -> [a]
padValues num def vals 
  | num <= length vals = vals
  | otherwise = vals ++ take (num - length vals) (repeat def)

toCSVData :: [Episode] -> [OutcomesRow] -> T.Text
toCSVData episodes outcomes = T.pack $ genCsvFile $ (fmap . fmap) T.unpack $ rectangular
  where expandOutcome (OutcomesRow outcomesName outcomesVals) = outcomesName : (padValues (length episodes) "" outcomesVals)
        withName = (T.pack "participant") : episodes
        expandedOutcomes = (fmap expandOutcome outcomes) :: [[T.Text]]
        rectangular = withName : expandedOutcomes
        

export fileName (Just c) = writeFile fileName $ T.unpack c
export fileName Nothing = return ()

exportFullData :: FullExportData -> IO ()
exportFullData = undefined

exportCSV :: String -> Int -> String -> Validated -> IO ()
exportCSV showName season stub validated = do
  let fileName = "../data/" ++ showName ++ "/" ++ stub ++ show season ++ ".csv"
  let csv = toCSVValidated validated
  export fileName csv
  return ()

-- Parse all episodes and export them
parseAll :: ScrapeData -> IO [String]
parseAll (ScrapeData (DownloadData url cleanName) seasonParser seasons) = do
  createDirectoryIfMissing True $ "../data/" ++ cleanName
  results <- mapM (\season -> parseOne cleanName seasonParser season) seasons
  let fullRow = cleanName : results
  return fullRow

-- Download the html to /data
downloadAll :: ScrapeData -> IO ()
downloadAll (ScrapeData (DownloadData url cleanName) _ seasons) = mapM_ (downloadOne url cleanName) seasons

doAll :: ScrapeData -> IO [String]
doAll dat = downloadAll dat >> parseAll dat

runAll :: IO ()
runAll = do
  results <- mapM doAll scrapeData
  let maxLength = maximum $ fmap length results
  let firstRow = "show" : fmap show [1..(maxLength - 1)]
  let padded = fmap (padValues maxLength "") results
  let csv = genCsvFile $ firstRow : padded
  writeFile "../data/summary.csv" csv
  return ()

getUrl :: String -> Int -> String
getUrl name num = "https://en.wikipedia.org/" ++ name ++ show num ++ ")"

downloadOne :: String -> String -> Int -> IO ()
downloadOne name cleanedName num = do
  (_, html) <- curlGetString (getUrl name num) []
  writeFile ("../downloaded/" ++ cleanedName ++ "_" ++ show num ++ ".html") html
  return ()


