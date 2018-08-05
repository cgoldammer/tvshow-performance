{-# LANGUAGE OverloadedStrings, FlexibleContexts, ScopedTypeVariables #-}

module Common where

import Text.Read (readMaybe)
import Data.Maybe (catMaybes, isJust, listToMaybe)
import Control.Monad (join)
import Data.Validation (Validation(Failure, Success), _Failure, _Success)
import qualified Data.HashMap.Strict as HM 
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Map (Map, fromList)
import Data.Tuple (swap)
import Control.Lens ((#), Prism, Prism', prism, folded, to, only,(^.), (^?),ix, toListOf, (^..), _2, _3)
import Text.Taggy (Node(..), Element(..), eltChildren)
import Text.Taggy.Lens (allAttributed, html, element, elements, children, contents, content, allNamed, named, name)
import Data.List (transpose, groupBy, partition)
import Debug.Trace (traceShow, trace)


-- Expands any table cell with cellspan=x into x nodes, each with the same 
-- content as the initial cell. All other cells and nodes are unaffected.
expandSpan :: (Element -> Maybe Int) -> Element -> [Element]
expandSpan expander el = take number $ repeat el
  where number = maybe 1 id $ expander el

expandColSpan = expandSpan getColSpan
expandRowSpan = expandSpan getRowSpan

-- Expand the cellspan, but applied to a node
expandNode :: Node -> [Node]
expandNode (NodeContent c) = [NodeContent c]
expandNode (NodeElement el) = fmap NodeElement $ expandColSpan el

-- Obtain the colspan of an Element
getSpan :: String -> Element -> Maybe Int
getSpan name el = join $ fmap (readMaybe . T.unpack) $ HM.lookup (T.pack name) (eltAttrs el)

getColSpan = getSpan "colspan"
getRowSpan = getSpan "rowspan"

-- Obtain the colspan of a node
getNodeSpan :: Node -> Maybe Int
getNodeSpan (NodeContent c) = Nothing
getNodeSpan (NodeElement e) = getColSpan e

getElement :: Node -> Maybe Element
getElement (NodeElement e) = Just e
getElement (NodeContent _) = Nothing

getContent :: Node -> Maybe T.Text
getContent (NodeElement _) = Nothing
getContent (NodeContent t) = Just t

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

firstContentDefault :: Node -> T.Text
firstContentDefault node = maybe "" id $ firstContent node

makeOutcome :: Maybe OutcomeData -> Maybe OutcomesRow
makeOutcome Nothing = Nothing
makeOutcome (Just (OutcomeData Nothing _)) = Nothing
makeOutcome (Just (OutcomeData (Just contestant) outcomes)) = Just $ OutcomesRow contestant outcomes


cleanText :: T.Text -> T.Text
cleanText text = T.pack $ filter (\c -> c `notElem` unwanted) s
  where s = T.unpack text
        unwanted = ['\n']

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
  let rest = fmap firstContentDefault restCols

  return $ OutcomeData (fmap cleanText contestant) (fmap cleanText rest)

type HtmlTable = [Node]
type TableGetter = TL.Text -> Maybe HtmlTable
type TableParser = HtmlTable -> Validated


outcomeVals :: [T.Text]
outcomeVals = ["OUT", "ELIM"]

personVals :: [T.Text]
personVals = ["Hometown", "Current Residence", "Original Placement"]

containsValue :: [T.Text] -> [T.Text] -> Bool
containsValue content expected = any (\v -> elem v content) expected

tableType :: HtmlTable -> Maybe (Table HtmlTable)
tableType table
  | containsAgeTown = Just $ PersonTable table
  | containsOutcomes = Just $ OutcomeTable table
  | otherwise = Nothing
  where containsAgeTown = containsValue allC personVals
        containsOutcomes = containsValue allC outcomeVals
        allC = fmap cleanText $ allContents table

allContents :: HtmlTable -> [T.Text]
allContents nodes = concat . concat $ (fmap . fmap) allContent $ allNodes nodes

data FullExportData = FullExportData {
  fullName :: String
, fullSeason :: Int
, fullValidatedTables :: [Table Validated]
}

data AllTableParser = AllTableParser {
  tbPersonParser :: TableParser
, tbOutcomeParser :: TableParser
}

data Table a = PersonTable a | OutcomeTable a deriving (Eq, Show)


justType :: Table a -> Table ()
justType (PersonTable _) = PersonTable ()
justType (OutcomeTable _) = OutcomeTable ()

allNodes :: HtmlTable -> [[Node]]
allNodes table = fmap eltChildren $ catMaybes $ fmap getElement table

pickEarliest :: [Table HtmlTable] -> [Table HtmlTable]
pickEarliest tables = catMaybes $ fmap (safeIndex 0) $ groupBy (\t1 t2 -> justType t1 == justType t2) tables

allTables :: TL.Text -> [Table HtmlTable]
allTables text = pickEarliest $ catMaybes $ fmap (tableType . eltChildren) $ catMaybes $ fmap getElement $ catMaybes $ fmap ((safeIndex 0) . eltChildren) $ getTables text


parseTable :: AllTableParser -> Table HtmlTable -> Table Validated
parseTable (AllTableParser defaultParser _) (PersonTable t) = PersonTable $ defaultParser t
parseTable (AllTableParser _ parser) (OutcomeTable t) = OutcomeTable $ parser t

getHtmlTables :: Element -> HtmlTable
getHtmlTables = eltChildren 

getTables :: TL.Text -> [Element]
getTables text = (toListOf $ html . allNamed (only "table")) text

getTable :: String -> TableGetter
getTable tableName text = fmap eltChildren $ join $ fmap getElement $ join $ fmap (safeIndex 0 . eltChildren) $ listToMaybe $ (toListOf $ html . allAttributed (folded . only (T.pack tableName))) text

safeIndex :: Int -> [a] -> Maybe a
safeIndex num l = l ^? ix num

data DownloadData = DownloadData { downloadUrlPart :: String, downloadCleanName :: String }

data ScrapeData = ScrapeData {
  downloadData :: DownloadData
, seasonParser :: Int -> AllTableParser
, scrapeSeasons :: [Int]
}

allContent :: Node -> [T.Text]
allContent node = catMaybes [getContent node] ++ catMaybes (sequenceA (fmap elContent (node ^? element)))

elContent :: Element -> [T.Text]
elContent el = childCont ++ concat (fmap elContent childEls)
  where childEls = catMaybes $ fmap getElement children
        childCont = catMaybes $ fmap getContent children
        children = eltChildren el

gg :: Node -> [Element]
gg node = node ^.. elements

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

takeVals :: Int -> OutcomesRow -> OutcomesRow
takeVals num (OutcomesRow name vals) = OutcomesRow name $ take num vals

-- Unfortunately, there are tables in which the colspans add up to more than the
-- number of episodes. This displays sensibly in the browser, because browsers are extremely
-- forgiving, but it's not correct data. In this case, drop everything after the number of
-- episodes is reached.
equalizeLengths :: [OutcomesRow] -> [OutcomesRow]
equalizeLengths [] = []
equalizeLengths (header:rest) = header:(fmap (takeVals numOutcomes) rest)
  where numOutcomes = length $ outcomesVals header


-- The number of outcomes for the first contestant must equal
-- the numer of episodes
equalLengths :: [OutcomesRow] -> Validation [VError] [OutcomesRow]
equalLengths (header:rest) = if maxLength == numEpisodes
  then _Success # (header:rest)
  else _Failure # [NonEqualLength (numEpisodes, maxLength)]
  where numEpisodes = length $ outcomesVals header
        numOutcomes = fmap (length . outcomesVals) rest
        maxLength = maximum numOutcomes


-- Running the outcome rows through the checks
check :: [OutcomesRow] -> Validated
check [] = _Failure # [NoPlayerData]
check (first:[]) = _Failure # [NoPlayerData]
check outcomes@(header:rest) = pure (FullData episodes rest) <*
                               equalLengths outcomes <*
                               hasData outcomes
  where episodes = outcomesVals header

type RowSpans = [(Element, Int)]
type TableRow = [Element]

withRowSpan' :: (Int, Element) -> (Int, Element, Maybe Int)
withRowSpan' (pos, el) = (pos, el, getRowSpan el)

simplifySpan = undefined

simplifySpan' :: (Int, Element, Maybe Int) -> (Int, Element, Int)
simplifySpan' (p, el, Nothing) = (p, el, 1)
simplifySpan' (p, el, Just n) = (p, el, n)

splitBySpan :: TableRow -> (Spans, TableRow)
splitBySpan row = trace ("without" ++ show withoutSpan) $ (fmap simplifySpan' withSpan, fmap (^. _2) withoutSpan)
  where rowWithSpans = fmap withRowSpan' $ zip [0..] row
        (withSpan, withoutSpan) = partition (isJust . (^. _3)) rowWithSpans

expandRow'' = expandRow' []

expandRow' :: Spans -> [[Element]] -> [[Element]]
expandRow' _ [] = []
expandRow' spans (first:rest) = trace ("\nspans" ++ show spans) els : expandRow' newSpans rest
  where (els, newSpans) = expandRowWithSpan first spans

type Spans = [(Int, Element, Int)]

expandRowWithSpan :: [Element] -> Spans -> ([Element], Spans)
expandRowWithSpan els prevSpans = trace ("row" ++ show nonSpans) $(blendInSpans nonSpans spans, reduceSpans spans)
  where (newSpans, nonSpans) = splitBySpan els
        spans = prevSpans ++ newSpans
        
blendInSpans :: [Element] -> Spans -> [Element]
blendInSpans els [] = els
blendInSpans els ((pos, el, _):rest) = trace (show "after" ++ show els) blendInSpans blended rest
  where (before, after) = splitAt pos els
        blended = before ++ [el] ++ after

reduceSpans :: Spans -> Spans
reduceSpans spans = filter aboveZero reduced
  where aboveZero s = (s ^. _3) > 0
        reduced = fmap (\(p, el, n) -> (p, el, n - 1)) spans
  
expandRowSpans :: RowSpans -> [[Element]]
expandRowSpans spans = fmap expandRowSpan cells
  where cells = fmap fst spans

-- expandRow :: [[Element]] -> [[Element]]
-- expandRow [] = []
-- expandRow (firstRow:rest) = fullTable
--   where zipped = zip expandedRowSpans (firstRest : restBeforeMerge)
--         (restBeforeMerge, restAfterMerge) = splitAt ((length expandedRowSpans) - 1) rest
--         (rowSpans, firstRest) = splitByRowSpan firstRow
--         expandedRowSpans = transpose $ expandRowSpans rowSpans
--         merged = [spans ++ rests | (spans, rests) <- zipped]
--         fullTable = merged ++ restAfterMerge

-- Like expandRow, but for nodes
expandNodes :: [[Node]] -> [[Node]]
expandNodes nodes = (fmap . fmap) NodeElement $ expandRow'' asElements
  where asElements = (fmap catMaybes . (fmap . fmap) getElement) nodes

selectRows :: Int -> Int -> [[a]] -> [[a]]
selectRows rowHeader rowContent vals = catMaybes $ headerRow : contentRows
  where headerRow = safeIndex rowHeader vals
        contentRows = fmap Just $ drop rowContent vals

constMap :: [Int] -> String -> Map Int String
constMap seasons name = fromList $ fmap swap $ fmap ((,) name) seasons
