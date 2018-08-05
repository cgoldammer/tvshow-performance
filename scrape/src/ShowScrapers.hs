{-# LANGUAGE OverloadedStrings, FlexibleContexts, ScopedTypeVariables #-}

module ShowScrapers where

import Data.Tuple (swap)
import Prelude hiding (lookup)
import qualified Data.Text.Lazy as TL
import Text.Taggy (Node(..), Element(..), eltChildren)
import Data.Maybe (catMaybes, isJust, listToMaybe)
import Control.Monad (join)
import Data.Map (Map, fromList, assocs, toAscList, findWithDefault, lookup)
import Debug.Trace (traceShow)

import Common (TableParser, TableGetter, AllTableParser(..), Validated, check, makeOutcome, rowParse, getNodeSpan, safeIndex, getElement, getTable, DownloadData(..), ScrapeData(..), equalizeLengths, constMap, selectRows, expandNodes)

defaultParse :: TableParser
defaultParse tableRows = check content
  where nodes = fmap eltChildren $ catMaybes $ fmap getElement tableRows :: [[Node]]
        content = equalizeLengths $ catMaybes $ fmap (makeOutcome . rowParse 0) nodes

parseTable :: TableParser
parseTable tableRows = check content
  where chefNode = join $ safeIndex 1 <$> safeIndex 1 nodes
        colSpan = maybe 1 id $ join $ fmap getNodeSpan chefNode
        nodes = fmap eltChildren $ catMaybes $ fmap getElement tableRows :: [[Node]]
        content = equalizeLengths $ catMaybes $ fmap (makeOutcome . rowParse colSpan) nodes

parseText :: String -> TL.Text -> Maybe Validated
parseText tableName text = fmap parseTable $ getTable tableName text

parse :: Int -> AllTableParser
parse = const $ AllTableParser defaultParse parseTable

download = DownloadData "wiki/Hell%27s_Kitchen_(U.S._season_" "HellsKitchen"
scrapeDataHK = ScrapeData download parse [1..17]

seasonsFNS = [1..14]
parseFNS = const (AllTableParser defaultParse defaultParse)
downloadFNS = DownloadData "wiki/Food_Network_Star_(season_" "FoodNetworkStar"
scrapeDataFNS = ScrapeData downloadFNS parseFNS seasonsFNS

seasonsRP = [1..10]
parseRP = const (AllTableParser defaultParse defaultParse)
downloadRP = DownloadData "wiki/RuPaul%27s_Drag_Race_(season_" "RuPaul"
scrapeDataRP = ScrapeData downloadRP parseRP seasonsRP

parseTableTC :: TableParser
parseTableTC tableRows = check $ equalizeLengths $ catMaybes $ fmap (makeOutcome . rowParse colSpan) nodesSelected
  where 
        nodesSelected = selectRows 0 4 nodes
        chefNode = join $ safeIndex 1 <$> safeIndex 1 nodesSelected
        colSpan = maybe 1 id $ join $ fmap getNodeSpan chefNode
        nodes = fmap eltChildren $ catMaybes $ fmap getElement tableRows :: [[Node]]

seasonsTC = [1..15]
parseTC = const (AllTableParser parseTableTC parseTableTC)
downloadTC = DownloadData "wiki/Top_Chef_(season_" "TopChef"
scrapeDataTC = ScrapeData downloadTC parseTC seasonsTC

parseTableMC :: TableParser
parseTableMC tableRows = check content
  where colSpan = 1
        nodes = drop 1 $ expandNodes $ fmap eltChildren $ catMaybes $ fmap getElement tableRows :: [[Node]]
        content = equalizeLengths $ catMaybes $ fmap (makeOutcome . rowParse colSpan) nodes

seasonsMC = [1..8]
parseMC = const (AllTableParser defaultParse parseTableMC)
downloadMC = DownloadData "wiki/MasterChef_(U.S._season_" "MasterChef"
scrapeDataMC = ScrapeData downloadMC parseMC seasonsMC

