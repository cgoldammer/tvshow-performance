{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RankNTypes #-}

module Lib where

import Scrapers

lib :: IO ()
lib = mapM_ doAll scrapeData
  
