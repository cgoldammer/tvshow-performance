{-# LANGUAGE OverloadedStrings, FlexibleContexts, ScopedTypeVariables #-}

module TopChef where

import Data.Tuple (swap)
import Prelude hiding (lookup)
import qualified Data.Text.Lazy as TL
import Text.Taggy (Node(..), Element(..), eltChildren)
import Data.Maybe (catMaybes, isJust, listToMaybe)
import Control.Monad (join)
import Data.Map (Map, fromList, assocs, toAscList, findWithDefault, lookup)

import Common (AllTableParser(..), TableParser, TableGetter, Validated, check, makeOutcome, rowParse, getNodeSpan, safeIndex, getElement, getTable, DownloadData(..), ScrapeData(..), selectRows, equalizeLengths, constMap)

parseTableTC :: TableParser
parseTableTC tableRows = check $ equalizeLengths $ catMaybes $ fmap (makeOutcome . rowParse colSpan) nodesSelected
  where 
        nodesSelected = selectRows 0 4 nodes
        chefNode = join $ safeIndex 1 <$> safeIndex 1 nodesSelected
        colSpan = maybe 1 id $ join $ fmap getNodeSpan chefNode
        nodes = fmap eltChildren $ catMaybes $ fmap getElement tableRows :: [[Node]]

parseText :: String -> TL.Text -> Maybe Validated
parseText tableName text = fmap parseTableTC $ getTable tableName text

seasonsTC = [1..15]
parse = const (AllTableParser parseTableTC parseTableTC)
download = DownloadData "wiki/Top_Chef_(season_" "TopChef"
scrapeDataTC = ScrapeData download parse [1..15]
