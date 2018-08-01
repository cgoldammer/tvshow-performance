{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import Test.HUnit hiding (Node) 
import qualified Text.Blaze.Html4.Strict as B
import qualified Text.Blaze.Html4.Strict.Attributes as BA
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Text.Blaze.Renderer.Utf8 (renderMarkup)
import Control.Lens (to, only,(^?),ix, toListOf)
import Text.Taggy (Node(..), Element, toMarkup, render)
import Text.Taggy.Lens (html, elements, children, contents,allNamed)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy.Encoding (decodeUtf8With)
import qualified Data.Maybe as M
import Debug.Trace
import Data.List (span, transpose)
import Data.Maybe (isJust)

import Common
import Scrapers

main :: IO ()
main = do
  runTestTT allTests
  return ()

allTests = TestList [
  "Content tests" ~: contentTests,
  "Table tests" ~: tableExpandTests,
  "Row expand tests" ~: rowExpandTests,
  "Simplify tests" ~: simpleTests]

getCells :: T.Text -> [[Element]]
getCells = (fmap . fmap) (M.catMaybes . fmap getElement) $ toListOf $ to TL.fromStrict . html . allNamed (only "tr") . children

expandFunction :: (Element -> [Element]) -> [[Element]] -> [[Element]]
expandFunction f cells = fmap concat $ (fmap . fmap) f cells

getExpandedCells :: T.Text -> [[Element]]
getExpandedCells text = expandFunction expandColSpan $ getCells text

tableExpandShape = [(basicTable, [1]), (spanTable, [2]), (spanTable2, [2]), (rowSpan, [2, 1])]

tableExpandTest :: B.Markup -> [Int] -> Test
tableExpandTest markup number = TestCase $ assertEqual error number lengths
  where  error = show (toText markup)
         cells = getCells $ toText markup
         expanded = expandFunction expandColSpan cells
         lengths = fmap length expanded


tableExpandTests = fmap (uncurry tableExpandTest) tableExpandShape

toText :: B.Markup -> T.Text
toText = TE.decodeUtf8 . BSL.toStrict . renderMarkup

basicTable :: B.Markup
basicTable = B.table $ do
  B.tr $ do
    B.td $ "cell1"
  
spanTable :: B.Markup
spanTable = B.table $ do
  B.tr $ do
    B.td B.! B.customAttribute "colspan" "2" $ "cell1" 

spanTable2 :: B.Markup
spanTable2 = B.table $ do
  B.tr $ do
    B.th B.! B.customAttribute "colspan" "2" $ "cell1" 

-- If there is a rowspan encountered, keep track of it and append those cells to the 
-- next encountered row.
rowSpan :: B.Markup
rowSpan = B.table $ do
  B.tr $ do
    B.td B.! B.customAttribute "rowspan" "2" $ "doubleRow"
    B.td $ "cell1"
  B.tr $ do
    B.td $ "cell2"

rowSpan0 :: B.Markup
rowSpan0 = B.table $ do
  B.tr $ do
    B.td B.! B.customAttribute "rowspan" "2" $ "doubleRow0"

rowExpandShape = [(rowSpan, [2, 2])]
rowExpandTest :: B.Markup -> [Int] -> Test
rowExpandTest markup number = TestCase $ assertEqual (show expanded) number lengths
  where  error = show expanded 
         cells = getCells $ toText markup
         expanded = expandRow cells
         lengths = fmap length expanded

rowExpandTests = fmap (uncurry rowExpandTest) rowExpandShape

topNode :: TL.Text -> Node
topNode = fmap NodeElement . fmap head . toListOf $ html . allNamed (only "div")

simpleData = [("<div><b>text</b></div>", "<div>text</div>"), ("<div>text</div>", "<div>text</div>") , ("<div></div>", "<div></div>")] 

simpleTest initial expected = TestCase $ assertEqual (show initial ++ " | " ++ sMarkup) simplified expectedNode
  where expectedNode = topNode expected
        simplified = simplifyNode $ topNode initial
        sMarkup = show $ render $ simplified

testContentData = [("<div><p>text1</p><p>text2</p></div>", ["text1", "text2"]), ("<div>text</div>", ["text"])]

testAllContent nodeText expected = TestCase $ assertEqual error expected content
  where node = topNode nodeText
        content = allContent node
        error = show (render node) ++ show content

contentTests = fmap (uncurry testAllContent) testContentData

simpleTests = fmap (uncurry simpleTest) simpleData


