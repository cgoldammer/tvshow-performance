{-# LANGUAGE OverloadedStrings, FlexibleContexts, ScopedTypeVariables #-}

module HellsKitchen where

import Data.Tuple (swap)
import Prelude hiding (lookup)
import qualified Data.Text.Lazy as TL
import Text.Taggy (Node(..), Element(..), eltChildren)
import Data.Maybe (catMaybes, isJust, listToMaybe)
import Control.Monad (join)
import Data.Map (Map, fromList, assocs, toAscList, findWithDefault, lookup)

import Common (TableParser, TableGetter, Validated, check, makeOutcome, rowParse, getNodeSpan, safeIndex, getElement, getTable, ParseSeason(..), DownloadData(..), ScrapeData(..))

parseTable :: TableParser
parseTable tableRows = check $ catMaybes $ fmap (makeOutcome . rowParse colSpan) nodes
  where chefNode = join $ safeIndex 1 <$> safeIndex 1 nodes
        colSpan = maybe 1 id $ join $ fmap getNodeSpan chefNode
        nodes = fmap eltChildren $ catMaybes $ fmap getElement tableRows :: [[Node]]

parseText :: String -> TL.Text -> Maybe Validated
parseText tableName text = fmap parseTable $ getTable tableName text

getterForSeason :: Int -> Maybe TableGetter
getterForSeason season = fmap getTable $ lookup season seasonMap
  where seasonMap = fromList $ fmap swap $ fmap ((,) "wikitable") [1..9] ++ fmap ((,) "wikitable plainrowheaders") [10..17]

parse = ParseSeason getterForSeason (const parseTable)
download = DownloadData "wiki/Hell%27s_Kitchen_(U.S._season_" "HellsKitchen"
scrapeDataHK = ScrapeData download parse [1..17]

