{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Zureg.Hackathon.MuniHac2020.Model
    ( TrackInterest (..)
    , ContributorLevel (..)
    , Project (..)
    , RegisterInfo (..)
    , csvHeader
    ) where

import qualified Data.Aeson.TH.Extended as A
import           Data.Csv               as Csv
import qualified Data.HashMap.Strict    as HM
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

data TrackInterest = TrackInterest
    { tiBeginner     :: !Bool
    , tiIntermediate :: !Bool
    , tiAdvanced     :: !Bool
    , tiGhcDevOps    :: !Bool
    } deriving (Eq, Show)

data ContributorLevel = ContributorLevel
    { clBeginner     :: !Bool
    , clIntermediate :: !Bool
    , clAdvanced     :: !Bool
    } deriving (Eq, Show)

data Project = Project
    { pName             :: !(Maybe T.Text)
    , pWebsite          :: !(Maybe T.Text)
    , pShortDescription :: !(Maybe T.Text)
    , pContributorLevel :: !ContributorLevel
    } deriving (Eq, Show)

data RegisterInfo = RegisterInfo
    { riAskMeAbout    :: !(Maybe T.Text)
    , riRegion        :: !(Maybe Region)
    , riTrackInterest :: !TrackInterest
    , riProject       :: !Project
    } deriving (Eq, Show)

$(A.deriveJSON A.options ''Region)
$(A.deriveJSON A.options ''TrackInterest)
$(A.deriveJSON A.options ''ContributorLevel)
$(A.deriveJSON A.options ''Project)
$(A.deriveJSON A.options ''RegisterInfo)

instance Csv.ToNamedRecord Project where
    toNamedRecord Project {..}
        = HM.unions [ namedRecord [ "Project Name"              .= pName
                                  , "Project Website"           .= pWebsite
                                  , "Project Short Description" .= pShortDescription
                                  ]
                    , toNamedRecord pContributorLevel
                    ]

instance Csv.ToNamedRecord ContributorLevel where
    toNamedRecord ContributorLevel {..}
        = namedRecord [ "CL Beginner"     .= clBeginner
                      , "CL Intermediate" .= clIntermediate
                      , "CL Advanced"     .= clAdvanced
                      ]

instance Csv.ToField Region where
    toField = toField . show

instance Csv.ToNamedRecord RegisterInfo where
    toNamedRecord RegisterInfo {..}
        = HM.unions [ namedRecord [ "AskMeAbout"         .= riAskMeAbout
                                  , "Region"             .= riRegion
                                  , "Beginner Track"     .= tiBeginner riTrackInterest
                                  , "Intermediate Track" .= tiIntermediate riTrackInterest
                                  , "Advanced Track"     .= tiAdvanced riTrackInterest
                                  , "GhcDevOps Track"    .= tiGhcDevOps riTrackInterest
                                  ]
                    , toNamedRecord riProject
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
    , "Beginner Track"
    , "Intermediate Track"
    , "Advanced Track"
    , "GhcDevOps Track"
    , "Project Name"
    , "Project Website"
    , "Project Short Description"
    , "CL Beginner"
    , "CL Intermediate"
    , "CL Advanced"
    , "Registered At"
    ]
