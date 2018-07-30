{-# LANGUAGE OverloadedStrings, FlexibleContexts, ScopedTypeVariables #-}

module Common where

import Text.Read (readMaybe)
import Data.Maybe (catMaybes, isJust, listToMaybe)
import Control.Monad (join)
import Data.Validation (Validation(Failure, Success), _Failure, _Success)
import qualified Data.HashMap.Strict as HM 
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
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

type HtmlTable = [Node]
type TableGetter = TL.Text -> Maybe HtmlTable
type TableParser = HtmlTable -> Validated


getTable :: String -> TableGetter
getTable tableName text = fmap eltChildren $ join $ fmap getElement $ join $ fmap (safeIndex 0 . eltChildren) $ listToMaybe $ (toListOf $ html . allAttributed (folded . only (T.pack tableName))) text

safeIndex :: Int -> [a] -> Maybe a
safeIndex num l = l ^? ix num

data DownloadData = DownloadData { downloadUrlPart :: String, downloadCleanName :: String }

data ParseSeason = ParseSeason { 
  seasonGetter :: Int -> Maybe TableGetter
, seasonParser :: Int -> TableParser
}

data ScrapeData = ScrapeData {
  downloadData :: DownloadData
, parseSeason :: ParseSeason
, scrapeSeasons :: [Int]
}

allContent :: Node -> [T.Text]
allContent node = concat [node ^.. elements . contents] ++ catMaybes [node ^? element . contents]

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

