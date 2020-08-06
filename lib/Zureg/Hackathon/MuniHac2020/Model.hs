{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Zureg.Hackathon.MuniHac2020.Model
    ( ExpertiseLevel (..)
    , RegisterInfo (..)
    , csvHeader
    ) where

import qualified Data.Aeson.TH.Extended as A
import           Data.Csv               as Csv
import qualified Data.Text              as T
import           Zureg.Model.Csv        ()

data Region
    = Germany
    | Europe
    | Asia
    | MiddleEast
    | NorthAmerica
    | CentralAmerica
    | SouthAmerica
    | Oceania
    deriving (Bounded, Enum, Eq, Show)

data ExpertiseLevel = Beginner | Advanced
    deriving (Bounded, Enum, Eq, Show)

data RegisterInfo = RegisterInfo
    { riAskMeAbout     :: !(Maybe T.Text)
    , riRegion         :: !(Maybe Region)
    , riExpertiseLevel :: !(Maybe ExpertiseLevel)
    } deriving (Eq, Show)

$(A.deriveJSON A.options ''Region)
$(A.deriveJSON A.options ''ExpertiseLevel)
$(A.deriveJSON A.options ''RegisterInfo)

instance Csv.ToField ExpertiseLevel where
    toField = toField . show

instance Csv.ToField Region where
    toField = toField . show

instance Csv.ToNamedRecord RegisterInfo where
    toNamedRecord RegisterInfo {..}
        = namedRecord [ "AskMeAbout"         .= riAskMeAbout
                      , "Region"             .= riRegion
                      , "Expertise Level"    .= riExpertiseLevel
                      ]

csvHeader :: Csv.Header
csvHeader = Csv.header
    [ "UUID"
    , "State"
    , "Scanned"
    , "Name"
    , "Name on Badge"
    , "Email"
    , "Affiliation"
    , "AskMeAbout"
    , "Region"
    , "Expertise Level"
    , "Registered At"
    ]
