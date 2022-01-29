{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Zureg.Hackathon.ZuriHac2022.Model
    ( TShirtCut (..)
    , TShirtSize (..)
    , TShirtInfo (..)
    , Region (..)
    , Occupation (..)
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

data TShirtCut = Female | Male deriving (Bounded, Enum, Eq, Show)

data TShirtSize = S | M | L | XL | XXL deriving (Bounded, Enum, Eq, Show)

data TShirtInfo = TShirtInfo
    { tsiCut  :: TShirtCut
    , tsiSize :: TShirtSize
    } deriving (Eq, Show)

data Region
    = Switzerland
    | Europe
    | Africa
    | AmericaCentral
    | AmericaNorth
    | AmericaSouth
    | Asia
    | MiddleEast
    | Oceania
    deriving (Bounded, Enum, Eq, Show)

data Occupation
    = Student
    | Tech
    | Academia
    | Other
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
    { riTShirt     :: !(Maybe TShirtInfo)
    , riRegion     :: !(Maybe Region)
    , riOccupation :: !(Maybe Occupation)
    , riProject    :: !Project
    } deriving (Eq, Show)

$(A.deriveJSON A.options ''TShirtSize)
$(A.deriveJSON A.options ''TShirtCut)
$(A.deriveJSON A.options ''TShirtInfo)
$(A.deriveJSON A.options ''Region)
$(A.deriveJSON A.options ''Occupation)
$(A.deriveJSON A.options ''ContributorLevel)
$(A.deriveJSON A.options ''Project)
$(A.deriveJSON A.options ''RegisterInfo)

instance Csv.ToField TShirtCut where
    toField = toField . show

instance Csv.ToField TShirtSize where
    toField = toField . show

instance Csv.ToNamedRecord TShirtInfo where
    toNamedRecord TShirtInfo {..} =
        namedRecord [ "T-Shirt Cut"  .= tsiCut
                    , "T-Shirt Size" .= tsiSize
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

instance Csv.ToField Region where
    toField = toField . show

instance Csv.ToField Occupation where
    toField = toField . show

instance Csv.ToNamedRecord RegisterInfo where
    toNamedRecord RegisterInfo {..}
        = HM.unions [ namedRecord [ "Region"     .= riRegion
                                  , "Occupation" .= riOccupation
                                  ]
                    , toNamedRecord riProject
                    ]

csvHeader :: Csv.Header
csvHeader = Csv.header
    [ "UUID"
    , "State"
    , "Scanned"
    , "Name"
    , "Email"
    , "Region"
    , "Occupation"
    , "Project Name"
    , "Project Website"
    , "Project Short Description"
    , "CL Beginner"
    , "CL Intermediate"
    , "CL Advanced"
    , "Registered At"
    ]