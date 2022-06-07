{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Zureg.Hackathon.MuniHac2022.Model
    ( ExpertiseLevel (..)
    , RegisterInfo (..)
    , csvHeader
    ) where

import qualified Data.Aeson.TH.Extended as A
import           Data.Csv               as Csv
import qualified Data.Text              as T
import           Zureg.Model.Csv        ()

data ExpertiseLevel = Beginner | Advanced
    deriving (Bounded, Enum, Eq, Show)

data RegisterInfo = RegisterInfo
    { riAskMeAbout     :: !(Maybe T.Text)
    , riExpertiseLevel :: !(Maybe ExpertiseLevel)
    , riKeepMePosted   :: !Bool
    } deriving (Eq, Show)

$(A.deriveJSON A.options ''ExpertiseLevel)
$(A.deriveJSON A.options ''RegisterInfo)

instance Csv.ToField ExpertiseLevel where
    toField = toField . show

instance Csv.ToNamedRecord RegisterInfo where
    toNamedRecord RegisterInfo {..}
        = namedRecord [ "AskMeAbout"         .= riAskMeAbout
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
    , "Expertise Level"
    , "Registered At"
    ]
