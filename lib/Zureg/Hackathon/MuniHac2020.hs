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
        , Hackathon.baseUrl = "https://registration.munihac.de"
        , Hackathon.contactUrl = "https://munihac.de/2020.html#contact"
        , Hackathon.slackUrl = "https://join.slack.com/t/munihac/shared_invite/zt-gaq3veyb-u3j9F0LqN0Q60Zc2MVqvSw"
        , Hackathon.legalNoticeUrl = Just "https://munihac.de/impressum.html"
        , Hackathon.capacity = 300
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
            , ReCaptcha.cSiteKey   = "6Lcmk7wZAAAAAKMmP6sKNvd5gVI8aGaMrWjE3JkZ"
            , ReCaptcha.cSecretKey = reCaptchaSecret
            }
        , Hackathon.scannerSecret = scannerSecret
        }
