module Analyze.Html where

import Analyze.RFrame (RFrame(..))

import qualified Lucid as L

-- TODO just a stub
renderHtml :: RFrame k v -> L.Html ()
renderHtml _ =
  L.table_ $ do
    L.tr_ $ do
      L.th_ "hi"
      L.th_ "bye"
    L.tr_ $ do
      L.td_ "1.1"
      L.td_ "1.2"
    L.tr_ $ do
      L.td_ "2.1"
      L.td_ "2.2"
