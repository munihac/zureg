-- | Discord integration for ZuriHac 2020.
{-# LANGUAGE OverloadedStrings #-}
module Zureg.Hackathon.ZuriHac2020.Discord
    ( credentialsFromEnv
    , aboutMe
    ) where

import qualified Data.Aeson              as Aeson
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Network.HTTP.Client     as Http
import qualified Network.HTTP.Client.TLS as Http
import           System.Environment      (getEnv)

newtype Credentials = Credentials T.Text

credentialsFromEnv :: IO Credentials
credentialsFromEnv =
    Credentials . T.pack <$> getEnv "ZUREG_DISCORD_ACCESS_TOKEN"

apiEndpoint :: T.Text
apiEndpoint = "https://discordapp.com/api/v6"

prepareRequest :: Credentials -> Http.Request -> Http.Request
prepareRequest (Credentials token) req = req
    { Http.requestHeaders =
        ("Accept", "application/json") :
        ("Authorization", T.encodeUtf8 $ "Bot " <> token) :
        Http.requestHeaders req
    }

getRequest :: Aeson.FromJSON a => Credentials -> T.Text -> IO a
getRequest creds path = do
    manager <- Http.newTlsManager
    req0 <- Http.parseRequest $ T.unpack $ apiEndpoint <> path
    let req = prepareRequest creds req0
    response <- Http.httpLbs req manager
    either (fail . ("Error decoding request: " ++)) pure $
        Aeson.eitherDecode' (Http.responseBody response)

aboutMe :: Credentials -> IO Aeson.Value
aboutMe creds = getRequest creds "/oauth2/applications/@me"
