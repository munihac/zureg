{-# LANGUAGE OverloadedStrings #-}
module Zureg.Hackathon.MuniHac2020.Form
    ( additionalInfoForm
    , additionalInfoView
    ) where

import qualified Data.Text                         as T
import qualified Text.Blaze.Html5                  as H
import qualified Text.Digestive                    as D
import qualified Text.Digestive.Blaze.Html5        as DH
import           Zureg.Hackathon.MuniHac2020.Model as MH20

additionalInfoForm :: Monad m => D.Form H.Html m MH20.RegisterInfo
additionalInfoForm = RegisterInfo
    <$> "askMeAbout" D..: optionalText
    <*> "region" D..: D.choice (
            (Nothing, "I'd rather not say") :
            [(Just s, H.toHtml $ show s) | s <- [minBound .. maxBound]])
            (Just Nothing)
    <*> "expertiseLevel" D..: D.choice
            [ (Nothing, "I'd rather not say")
            , (Just Beginner, "I've just started learning Haskell")
            , (Just Advanced, "I know my way around Haskell") ]
            (Just Nothing)
  where
    optionalText =
        (\t -> let t' = T.strip t in if T.null t' then Nothing else Just t') <$>
        (D.text Nothing)

additionalInfoView :: D.View H.Html -> H.Html
additionalInfoView view = do
    H.h2 "Optional information"
    DH.label "askMeAbout" view $ H.strong "Ask me about"
    H.p $ do
        "Topic(s) that you want to talk about with outhers.  It's a good ice "
        "breaker for people who want to chat with you."
    DH.inputText "askMeAbout" view
    H.br

    H.p $ H.strong "Region"
    DH.label "region" view $ do
        "From what area will you attend MuniHac remotely?  This is for getting "
        "an overview over the different timezones."
    DH.inputSelect "region" view
    H.br

    H.p $ H.strong "Your Level of Expertise"
    DH.label "expertiseLevel" view $
        "Let us know whether you're a Haskell beginner or expert!"
    DH.inputSelect "expertiseLevel" view
    H.br
