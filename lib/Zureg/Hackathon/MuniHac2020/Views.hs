{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Zureg.Hackathon.MuniHac2020.Views
    ( ticketView
    , scanView
    ) where

import           Data.List                         (intercalate)
import qualified Text.Blaze.Html5                  as H

import           Zureg.Hackathon.MuniHac2020.Model as MH20

ticketView :: MH20.RegisterInfo -> H.Html
ticketView RegisterInfo {..} = do
    "Track interest(s): "
    H.toHtml $ intercalate ", " $
        ["Beginner" | tiBeginner riTrackInterest]
        ++ ["Intermediate" | tiIntermediate riTrackInterest]
        ++ ["Advanced" | tiAdvanced riTrackInterest]
        ++ ["GHC DevOps" | tiGhcDevOps riTrackInterest]

scanView :: MH20.RegisterInfo -> H.Html
scanView RegisterInfo {..} = mempty
