{-# LANGUAGE OverloadedStrings, FlexibleContexts, ScopedTypeVariables #-}

module Scrapers where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeUtf8With, decodeUtf8)
import Data.ByteString.Lazy as BSL (readFile, ByteString, toStrict)

import Text.Read (readMaybe)
import Data.Maybe (catMaybes, isJust, listToMaybe)
import Control.Monad (join)
import qualified Data.HashMap.Strict as HM 
import Data.Map (Map, fromList, assocs, toAscList, findWithDefault)
import Debug.Trace
import Data.Validation (Validation(Failure, Success), _Failure, _Success)
import Network.Curl (curlGetString)
import Data.Tuple (swap)

import Control.Lens ((#), Prism, Prism', prism, folded, to, only,(^.), (^?),ix, toListOf, (^..))
import Text.Taggy (Node(..), Element(..), eltChildren)
import Text.Taggy.Lens (allAttributed, html, element, elements, children, contents, content, allNamed, named, name)

-- Expands any table cell with cellspan=x into x nodes, each with the same 
-- content as the initial cell. All other cells and nodes are unaffected.
expandCellspan :: Element -> [Element]
expandCellspan el = take number $ repeat el
  where number = maybe 1 id $ getColSpan el

-- Expand the cellspan, but applied to a node
expandNode :: Node -> [Node]
expandNode (NodeContent c) = [NodeContent c]
expandNode (NodeElement el) = fmap NodeElement $ expandCellspan el

-- Obtain the colspan of an Element
getColSpan :: Element -> Maybe Int
getColSpan el = join $ fmap (readMaybe . T.unpack) $ HM.lookup (T.pack "colspan") (eltAttrs el)

-- Obtain the colspan of a node
getNodeSpan :: Node -> Maybe Int
getNodeSpan (NodeContent c) = Nothing
getNodeSpan (NodeElement e) = getColSpan e

getElement :: Node -> Maybe Element
getElement (NodeElement e) = Just e
getElement (NodeContent _) = Nothing

-- The main outcomes of interest
data OutcomesRow = OutcomesRow {
  outcomesName :: T.Text
, outcomesVals :: [T.Text]
} deriving (Show)

allContent :: Node -> [T.Text]
allContent node = concat [node ^.. elements . contents] ++ catMaybes [node ^? element . contents]

-- In case the node has only content or no content, it stays unchanged. 
-- In case it has children that have content, return a new node that
-- contains the first non-missing content.
simplifyNode :: Node -> Node
simplifyNode (NodeContent t) = NodeContent t
simplifyNode node@(NodeElement (Element name attrs children)) = NodeElement $ Element name attrs simpleChildren
  where contents = allContent node
        firstContent = catMaybes [listToMaybe contents] -- [Text]
        simpleChildren = fmap NodeContent firstContent

firstContent :: Node -> Maybe T.Text
firstContent = listToMaybe . allContent


makeOutcome :: Maybe OutcomeData -> Maybe OutcomesRow
makeOutcome Nothing = Nothing
makeOutcome (Just (OutcomeData Nothing _)) = Nothing
makeOutcome (Just (OutcomeData (Just contestant) outcomes)) = Just $ OutcomesRow contestant outcomes

data OutcomeData = OutcomeData (Maybe T.Text) [T.Text] deriving Show

-- This is the main business logic to parse a row into `OutcomeData`.
-- This consists of expanding cells with colspan > 1, then picking
-- the name and the outcomes.
rowParse :: Int -> [Node] -> Maybe OutcomeData
rowParse startCol row = do
  -- todo: Get cell span, and use that to determine startcol
  let expanded = concat $ fmap expandNode row
  let total = length expanded
  contestantCol :: Node <- expanded ^? ix startCol
  let contestant = firstContent contestantCol
  restCols :: [Node] <- mapM (\num -> expanded ^? ix num) [(startCol+1)..(total - 1)]
  let rest = catMaybes $ fmap firstContent restCols
  return $ OutcomeData contestant rest

getFile :: String -> IO TL.Text
getFile name = fmap decodeUtf8 $ BSL.readFile $ name

getData :: String -> Int -> IO TL.Text
getData name num = getFile $ "data/downloaded/" ++ name ++ "_" ++ show num ++ ".html"

safeIndex :: Int -> [a] -> Maybe a
safeIndex num l = l ^? ix num

parseNodes tableName = toListOf $ html . allAttributed (folded . only (T.pack tableName)) . allNamed (only (T.pack "tr")) . children

parseOne :: String -> Int -> String -> IO ()
parseOne name num tableName = do
  fileText <- getData name num
  let nodes = parseNodes tableName fileText

  -- Obtaining the node that tells us the colspan
  let chefNode = join $ safeIndex 1 <$> safeIndex 1 nodes
  let colSpan = maybe 1 id $ join $ fmap getNodeSpan chefNode

  -- Parsing the data into validated format.
  let full = check $ catMaybes $ fmap (makeOutcome . rowParse colSpan) nodes
  print $ show num ++ ": " ++ " " ++ show full

  -- Exporting the data
  exportCSV name num full
  return ()
  
type Episode = T.Text 
type Episodes = [Episode]

data FullData = FullData Episodes [OutcomesRow] deriving Show

getEpisodes :: T.Text -> OutcomesRow -> Maybe [T.Text]
getEpisodes headerName (OutcomesRow header names) = if (header == headerName) then Just names else Nothing

-- Business logic to validate outcome rows

type Validated = Validation [VError] FullData
data VError = NonEqualLength (Int, Int) | NoPlayerData deriving (Show)

-- The outcomes must contain data
hasData :: [OutcomesRow] -> Validation [VError] [OutcomesRow]
hasData [] = _Failure # [NoPlayerData]
hasData (header:[]) = _Failure # [NoPlayerData]
hasData (header:first) = _Success # (header:first)

-- The number of outcomes for the first contestant must equal
-- the numer of episodes
equalLengths :: [OutcomesRow] -> Validation [VError] [OutcomesRow]
equalLengths (header:first:rest) = if numOutcomes == numEpisodes
  then _Success # (header:first:rest)
  else _Failure # [NonEqualLength (numEpisodes, numOutcomes)]
  where numEpisodes = length $ outcomesVals header
        numOutcomes = length $ outcomesVals first

-- Running the outcome rows through the checks
check :: [OutcomesRow] -> Validated
check [] = _Failure # [NoPlayerData]
check (first:[]) = _Failure # [NoPlayerData]
check outcomes@(header:rest) = pure (FullData episodes rest) <*
                               equalLengths outcomes <*
                               hasData outcomes
  where episodes = outcomesVals header

getUrl :: String -> Int -> String
getUrl name num = "https://en.wikipedia.org/" ++ name ++ show num ++ ")"

downloadOne :: String -> String -> Int -> IO ()
downloadOne name cleanedName num = do
  (_, html) <- curlGetString (getUrl name num) []
  writeFile ("data/downloaded/" ++ cleanedName ++ "_" ++ show num ++ ".html") html
  return ()

-- Each season has some corresponding data that is required
-- for parsing the html. For instance, right now this is
-- the class name of the table containing the outcome data
data SeasonData = SeasonData { seasonString :: String }

data ScrapeData = ScrapeData {
  scrapeCleanName :: String
, scrapeName :: String
, scrapeSeasons :: Map Int SeasonData
}

scrapeValues = fromList $ fmap swap $ fmap ((,) (SeasonData "wikitable")) [1..9] ++ fmap ((,) (SeasonData "wikitable plainrowheaders")) [10..17]
scrapeDataHellsKitchen = ScrapeData "HellsKitchen" "wiki/Hell%27s_Kitchen_(U.S._season_" scrapeValues

scrapeData = [scrapeDataHellsKitchen]

-- IO actions to download and export the data and related functions


toCSVValidated :: Validated -> Maybe T.Text
toCSVValidated (Failure _) = Nothing
toCSVValidated (Success (FullData episodes outcomes)) = Just $ toCSVData episodes outcomes

toCSVData :: [Episode] -> [OutcomesRow] -> T.Text
toCSVData episodes outcomes = T.intercalate newLine $ fmap commaFold (withName : expandedOutcomes)
  where comma = T.pack ","
        withName = (T.pack "participant") : episodes
        commaFold = T.intercalate comma
        expandOutcome (OutcomesRow outcomesName outcomesVals) = outcomesName : outcomesVals
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
parseAll (ScrapeData cleanedName _ seasons) = do
  mapM_ (\(num, seasonDat) -> parseOne cleanedName num (seasonString seasonDat)) $ assocs seasons

-- Download the html to /data
downloadAll :: ScrapeData -> IO ()
downloadAll (ScrapeData cleanedName name seasons) = do
  let fullScrapeData = [(name, cleanedName, season) | (season, _) <- toAscList seasons]
  mapM_ (\(name, cleanedName, season) -> downloadOne name cleanedName season) fullScrapeData

doAll :: ScrapeData -> IO ()
doAll dat = downloadAll dat >> parseAll dat

