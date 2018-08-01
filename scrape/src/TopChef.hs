{-# LANGUAGE OverloadedStrings, FlexibleContexts, ScopedTypeVariables #-}

module TopChef where

import Data.Tuple (swap)
import Prelude hiding (lookup)
import qualified Data.Text.Lazy as TL
import Text.Taggy (Node(..), Element(..), eltChildren)
import Data.Maybe (catMaybes, isJust, listToMaybe)
import Control.Monad (join)
import Data.Map (Map, fromList, assocs, toAscList, findWithDefault, lookup)

import Common (TableParser, TableGetter, Validated, check, makeOutcome, rowParse, getNodeSpan, safeIndex, getElement, getTable, ParseSeason(..), DownloadData(..), ScrapeData(..), selectRows)

parseTableTC :: TableParser
parseTableTC tableRows = check $ catMaybes $ fmap (makeOutcome . rowParse colSpan) nodesSelected
  where 
        nodesSelected = selectRows 0 4 nodes
        chefNode = join $ safeIndex 1 <$> safeIndex 1 nodesSelected
        colSpan = maybe 1 id $ join $ fmap getNodeSpan chefNode
        nodes = fmap eltChildren $ catMaybes $ fmap getElement tableRows :: [[Node]]

parseText :: String -> TL.Text -> Maybe Validated
parseText tableName text = fmap parseTableTC $ getTable tableName text

getterForSeason :: Int -> Maybe TableGetter
getterForSeason season = fmap getTable $ lookup season seasonMap
  where seasonMap = fromList $ fmap swap $ fmap ((,) "wikitable") [1..15]

parse = ParseSeason getterForSeason (const parseTableTC)
download = DownloadData "wiki/Top_Chef_(season_" "TopChef"
scrapeDataTC = ScrapeData download parse [1..15]
