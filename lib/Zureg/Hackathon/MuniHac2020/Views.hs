{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Zureg.Hackathon.MuniHac2020.Views
    ( ticketView
    , scanView
    ) where

import qualified Text.Blaze.Html5                  as H

import           Zureg.Hackathon.MuniHac2020.Model as MH20

ticketView :: MH20.RegisterInfo -> H.Html
ticketView RegisterInfo {..} = mempty

scanView :: MH20.RegisterInfo -> H.Html
scanView RegisterInfo {..} = mempty
