{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Zureg.Hackathon.MuniHac2022.Model
    ( TShirtInfo (..)
    , TShirtCut (..)
    , TShirtSize (..)
    , Region (..)
    , ExpertiseLevel (..)
    , Project (..)
    , ContributorLevel (..)
    , RegisterInfo (..)
    , csvHeader
    ) where

import qualified Data.Aeson.TH.Extended as A
import           Data.Csv               as Csv
import qualified Data.HashMap.Strict    as HM
import qualified Data.Text              as T
import           Zureg.Model.Csv        ()

data TShirtCut = Female | Male deriving (Bounded, Enum, Eq, Show)

data TShirtSize = S | M | L | XL | XXL deriving (Bounded, Enum, Eq, Show)

data TShirtInfo = TShirtInfo
    { tsiCut  :: TShirtCut
    , tsiSize :: TShirtSize
    } deriving (Eq, Show)

data Region
    = Germany
    | Europe
    | Africa
    | AmericaCentral
    | AmericaNorth
    | AmericaSouth
    | Asia
    | MiddleEast
    | Oceania
    deriving (Bounded, Enum, Eq, Show)

data ExpertiseLevel = Beginner | Advanced
    deriving (Bounded, Enum, Eq, Show)

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
    { riTShirt         :: !(Maybe TShirtInfo)
    , riExpertiseLevel :: !(Maybe ExpertiseLevel)
    , riAskMeAbout     :: !(Maybe T.Text)
    , riRegion         :: !(Maybe Region)
    , riProject        :: !Project
    , riKeepMePosted   :: !Bool
    } deriving (Eq, Show)

$(A.deriveJSON A.options ''TShirtSize)
$(A.deriveJSON A.options ''TShirtCut)
$(A.deriveJSON A.options ''TShirtInfo)
$(A.deriveJSON A.options ''Region)
$(A.deriveJSON A.options ''ExpertiseLevel)
$(A.deriveJSON A.options ''ContributorLevel)
$(A.deriveJSON A.options ''Project)
$(A.deriveJSON A.options ''RegisterInfo)

instance Csv.ToField TShirtCut where
    toField = toField . show

instance Csv.ToField TShirtSize where
    toField = toField . show

instance Csv.ToNamedRecord (Maybe TShirtInfo) where
    toNamedRecord mbTi =
        namedRecord [ "T-Shirt Cut"  .= (tsiCut <$> mbTi)
                    , "T-Shirt Size" .= (tsiSize <$> mbTi)
                    ]

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

instance Csv.ToField ExpertiseLevel where
    toField = toField . show

instance Csv.ToField Region where
    toField = toField . show

instance Csv.ToNamedRecord RegisterInfo where
    toNamedRecord RegisterInfo {..}
        = HM.unions
            [ namedRecord
                [ "AskMeAbout" .= riAskMeAbout
                , "Expertise Level" .= riExpertiseLevel
                , "Region" .= riRegion
                ]
            , toNamedRecord riProject
            , toNamedRecord riTShirt
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
    , "Expertise Level"
    , "AskMeAbout"
    , "Region"
    , "Project Name"
    , "Project Website"
    , "Project Short Description"
    , "CL Beginner"
    , "CL Intermediate"
    , "CL Advanced"
    , "Registered At"
    , "T-Shirt Cut"
    , "T-Shirt Size"
    ]
