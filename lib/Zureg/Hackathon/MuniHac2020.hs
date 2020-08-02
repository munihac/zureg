{-# LANGUAGE OverloadedStrings #-}
module Zureg.Hackathon.MuniHac2020
    ( newHackathon
    ) where

import qualified Data.Text                         as T
import           System.Environment                (getEnv)
import qualified Zureg.Database                    as Database
import           Zureg.Hackathon.Interface         (Hackathon)
import qualified Zureg.Hackathon.Interface         as Hackathon
import           Zureg.Hackathon.MuniHac2020.Form  as MH20
import           Zureg.Hackathon.MuniHac2020.Model as MH20
import           Zureg.Hackathon.MuniHac2020.Views as MH20
import qualified Zureg.ReCaptcha                   as ReCaptcha
import qualified Zureg.SendEmail                   as SendEmail

newHackathon :: IO (Hackathon RegisterInfo)
newHackathon = do
    scannerSecret   <- T.pack <$> getEnv "ZUREG_SCANNER_SECRET"
    reCaptchaSecret <- T.pack <$> getEnv "ZUREG_RECAPTCHA_SECRET"
    email           <- T.pack <$> getEnv "ZUREG_EMAIL"

    return Hackathon.Hackathon
        { Hackathon.name = "MuniHac 2020"
        , Hackathon.baseUrl = "TODO" -- "https://zureg.zfoh.ch"
        , Hackathon.contactUrl = "TODO" -- "https://zfoh.ch/zurihac2020/#contact"
        , Hackathon.slackUrl = "TODO" -- "https://slack.zurihac.info/"
        , Hackathon.capacity = 800
        , Hackathon.confirmation = False

        , Hackathon.registerForm = MH20.additionalInfoForm
        , Hackathon.registerView = MH20.additionalInfoView
        , Hackathon.ticketView = MH20.ticketView
        , Hackathon.scanView = MH20.scanView
        , Hackathon.csvHeader = MH20.csvHeader

        , Hackathon.databaseConfig = Database.defaultConfig
        , Hackathon.sendEmailConfig = SendEmail.Config
            { SendEmail.cFrom = "MuniHac Registration Bot <" <> email <> ">"
            }
        , Hackathon.reCaptchaConfig = ReCaptcha.Config
            { ReCaptcha.cEnabled   = True
            , ReCaptcha.cSiteKey   = "6LcVUm8UAAAAAL0ooPLkNT3O9oEXhGPK6kZ-hQk7"
            , ReCaptcha.cSecretKey = reCaptchaSecret
            }
        , Hackathon.scannerSecret = scannerSecret
        }
