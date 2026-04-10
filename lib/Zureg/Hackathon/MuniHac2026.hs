{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Zureg.Hackathon.MuniHac2026
    ( newHackathon
    ) where

import qualified Data.Text                         as T
import qualified Text.Blaze.Html5                  as H
import           System.Environment                (getEnv)
import qualified Zureg.Captcha.ReCaptcha           as ReCaptcha
import qualified Zureg.Database                    as Database
import           Zureg.Hackathon.Interface         (Hackathon)
import qualified Zureg.Hackathon.Interface         as Hackathon
import qualified Zureg.Hackathon.MuniHac2026.Form  as MH26
import qualified Zureg.Hackathon.MuniHac2026.Model as MH26
import           Zureg.Model
import qualified Zureg.SendEmail                   as SendEmail

newHackathon :: IO (Hackathon MH26.RegisterInfo)
newHackathon = do
    scannerSecret   <- T.pack <$> getEnv "ZUREG_SCANNER_SECRET"
    email           <- T.pack <$> getEnv "ZUREG_EMAIL"

    reCaptchaSecret <- T.pack <$> getEnv "ZUREG_RECAPTCHA_SECRET"
    captcha         <- ReCaptcha.new ReCaptcha.Config
        { ReCaptcha.cSiteKey   = "6Lcmk7wZAAAAAKMmP6sKNvd5gVI8aGaMrWjE3JkZ"
        , ReCaptcha.cSecretKey = reCaptchaSecret
        }

    return Hackathon.Hackathon
        { Hackathon.name = "MuniHac 2026"
        , Hackathon.baseUrl = "https://registration.munihac.de"
        , Hackathon.contactUrl = "https://munihac.de/2026.html#contact"
        , Hackathon.legalNoticeUrl = Just "https://munihac.de/impressum.html"
        , Hackathon.capacity = 80
        , Hackathon.confirmation = True

        , Hackathon.registerBadgeName = True
        , Hackathon.registerAffiliation = True

        , Hackathon.registerForm = MH26.additionalInfoForm
        , Hackathon.registerView = MH26.additionalInfoView
        , Hackathon.ticketView = mempty
        , Hackathon.scanView = \Registrant {..} -> case rAdditionalInfo of
            Nothing                -> mempty
            Just MH26.RegisterInfo {..} -> case riTShirt of
                Nothing                   -> "No T-Shirt"
                Just MH26.TShirtInfo {..} -> do
                    "T-Shirt size: "
                    H.strong $ H.toHtml (show tsiSize)
        , Hackathon.csvHeader = MH26.csvHeader

        , Hackathon.databaseConfig = Database.defaultConfig
        , Hackathon.sendEmailConfig = SendEmail.Config
            { SendEmail.cFrom = "MuniHac Registration Bot <" <> email <> ">"
            }
        , Hackathon.captcha = captcha
        , Hackathon.scannerSecret = scannerSecret
        , Hackathon.chatExplanation = H.p "You can join the MuniHac Slack instance here:"
        , Hackathon.chatUrl = pure "https://join.slack.com/t/munihac/shared_invite/zt-2i1v0wxev-M_DkjOxBpuX2B5CfPk8Nlg"
        }
