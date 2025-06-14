{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TemplateHaskell           #-}
module Zureg.Hackathon
    ( Hackathon (..)
    , withHackathonFromEnv
    ) where

import qualified Data.Aeson                  as Aeson
import qualified Data.Csv                    as Csv
import           Data.List                   (intercalate)
import           Data.Maybe                  (fromMaybe)
import           System.Environment          (lookupEnv)
import           Zureg.Hackathon.Interface
import qualified Zureg.Hackathon.MuniHac2020 as MuniHac2020
import qualified Zureg.Hackathon.MuniHac2022 as MuniHac2022
import qualified Zureg.Hackathon.MuniHac2024 as MuniHac2024
import qualified Zureg.Hackathon.MuniHac2025 as MuniHac2025
import qualified Zureg.Hackathon.ZuriHac2021 as ZuriHac2021
import qualified Zureg.Hackathon.ZuriHac2022 as ZuriHac2022
import qualified Zureg.Hackathon.ZuriHac2023 as ZuriHac2023
import qualified Zureg.Hackathon.ZuriHac2024 as ZuriHac2024

-- | Load the hackathon stored in the 'ZUREG_HACKATHON' environment variable.
withHackathonFromEnv
    :: (forall r. (Eq r, Csv.ToNamedRecord r, Aeson.FromJSON r, Aeson.ToJSON r)
        => Hackathon r -> IO a)
    -> IO a
withHackathonFromEnv f = do
    mbHackathonName <- lookupEnv "ZUREG_HACKATHON"
    sh <- fromMaybe (fail message) (mbHackathonName >>= flip lookup hackathons)
    case sh of SomeHackathon h -> f h
  where
    message =
        "Environment variable ZUREG_HACKATHON should be set to one of: " ++
        intercalate ", " (map fst hackathons)

data SomeHackathon =
       forall r. (Eq r, Csv.ToNamedRecord r, Aeson.FromJSON r, Aeson.ToJSON r)
    => SomeHackathon (Hackathon r)

hackathons :: [(String, IO SomeHackathon)]
hackathons =
    [ ("munihac2020", SomeHackathon <$> MuniHac2020.newHackathon)
    , ("munihac2022", SomeHackathon <$> MuniHac2022.newHackathon)
    , ("munihac2024", SomeHackathon <$> MuniHac2024.newHackathon)
    , ("munihac2025", SomeHackathon <$> MuniHac2025.newHackathon)
    , ("zurihac2021", SomeHackathon <$> ZuriHac2021.newHackathon)
    , ("zurihac2022", SomeHackathon <$> ZuriHac2022.newHackathon)
    , ("zurihac2023", SomeHackathon <$> ZuriHac2023.newHackathon)
    , ("zurihac2024", SomeHackathon <$> ZuriHac2024.newHackathon)
    ]
