{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RankNTypes #-}

module Lib where

import Scrapers

lib :: IO ()
lib = do
  mapM_ doAll scrapeData
  
