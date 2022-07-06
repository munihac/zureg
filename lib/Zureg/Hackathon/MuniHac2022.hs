{-# LANGUAGE OverloadedStrings #-}
module Zureg.Hackathon.MuniHac2022
    ( newHackathon
    ) where

import qualified Data.Text                         as T
import           System.Environment                (getEnv)
import qualified Zureg.Captcha.ReCaptcha           as ReCaptcha
import qualified Zureg.Database                    as Database
import           Zureg.Hackathon.Interface         (Hackathon)
import qualified Zureg.Hackathon.Interface         as Hackathon
import           Zureg.Hackathon.MuniHac2022.Form  as MH22
import           Zureg.Hackathon.MuniHac2022.Model as MH22
import qualified Zureg.SendEmail                   as SendEmail

newHackathon :: IO (Hackathon RegisterInfo)
newHackathon = do
    scannerSecret   <- T.pack <$> getEnv "ZUREG_SCANNER_SECRET"
    email           <- T.pack <$> getEnv "ZUREG_EMAIL"

    reCaptchaSecret <- T.pack <$> getEnv "ZUREG_RECAPTCHA_SECRET"
    captcha         <- ReCaptcha.new ReCaptcha.Config
        { ReCaptcha.cSiteKey   = "6Lcmk7wZAAAAAKMmP6sKNvd5gVI8aGaMrWjE3JkZ"
        , ReCaptcha.cSecretKey = reCaptchaSecret
        }

    return Hackathon.Hackathon
        { Hackathon.name = "MuniHac 2022"
        , Hackathon.baseUrl = "https://registration.munihac.de"
        , Hackathon.contactUrl = "https://munihac.de/2022.html#contact"
        , Hackathon.legalNoticeUrl = Just "https://munihac.de/impressum.html"
        , Hackathon.capacity = 100
        , Hackathon.confirmation = False

        , Hackathon.registerBadgeName = True
        , Hackathon.registerAffiliation = True

        , Hackathon.registerForm = MH22.additionalInfoForm
        , Hackathon.registerView = MH22.additionalInfoView
        , Hackathon.ticketView = mempty
        , Hackathon.scanView = mempty
        , Hackathon.csvHeader = MH22.csvHeader

        , Hackathon.databaseConfig = Database.defaultConfig
        , Hackathon.sendEmailConfig = SendEmail.Config
            { SendEmail.cFrom = "MuniHac Registration Bot <" <> email <> ">"
            }
        , Hackathon.captcha = captcha
        , Hackathon.scannerSecret = scannerSecret
        , Hackathon.chatUrl = pure "https://join.slack.com/t/munihac/shared_invite/zt-gaq3veyb-u3j9F0LqN0Q60Zc2MVqvSw"
        , Hackathon.chatExplanation = mempty
        }
