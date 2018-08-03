{-# LANGUAGE OverloadedStrings, FlexibleContexts, ScopedTypeVariables #-}

module HellsKitchen where

import Data.Tuple (swap)
import Prelude hiding (lookup)
import qualified Data.Text.Lazy as TL
import Text.Taggy (Node(..), Element(..), eltChildren)
import Data.Maybe (catMaybes, isJust, listToMaybe)
import Control.Monad (join)
import Data.Map (Map, fromList, assocs, toAscList, findWithDefault, lookup)
import Debug.Trace (traceShow)

import Common (TableParser, TableGetter, AllTableParser(..), Validated, check, makeOutcome, rowParse, getNodeSpan, safeIndex, getElement, getTable, DownloadData(..), ScrapeData(..), equalizeLengths, constMap)

defaultParse :: TableParser
defaultParse tableRows = check content
  where nameNode = 0
        nodes = fmap eltChildren $ catMaybes $ fmap getElement tableRows :: [[Node]]
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
parseFNS = const (AllTableParser defaultParse parseTable)
downloadFNS = DownloadData "wiki/Food_Network_Star_(season_" "FoodNetworkStar"
scrapeDataFNS = ScrapeData downloadFNS parseFNS seasonsFNS

seasonsRP = [1..10]
parseRP = const (AllTableParser defaultParse defaultParse)
downloadRP = DownloadData "wiki/RuPaul%27s_Drag_Race_(season_" "RuPaul"
scrapeDataRP = ScrapeData downloadRP parseRP seasonsRP
