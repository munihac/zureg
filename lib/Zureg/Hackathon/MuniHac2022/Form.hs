{-# LANGUAGE OverloadedStrings #-}
module Zureg.Hackathon.MuniHac2022.Form
    ( additionalInfoForm
    , additionalInfoView
    ) where

import qualified Data.Text                         as T
import qualified Text.Blaze.Html5                  as H
import qualified Text.Digestive                    as D
import qualified Text.Digestive.Blaze.Html5        as DH
import           Zureg.Hackathon.MuniHac2022.Model as MH22

additionalInfoForm :: Monad m => D.Form H.Html m MH22.RegisterInfo
additionalInfoForm = RegisterInfo
    <$> "askMeAbout" D..: optionalText
    <*> "expertiseLevel" D..: D.choice
            [ (Nothing, "I'd rather not say")
            , (Just Beginner, "I've just started learning Haskell")
            , (Just Advanced, "I know my way around Haskell") ]
            (Just Nothing)
    <*> "keepMePosted" D..: D.bool (Just False)
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

    H.p $ H.strong "Your Level of Expertise"
    DH.label "expertiseLevel" view $
        "Let us know whether you're a Haskell beginner or expert!"
    DH.inputSelect "expertiseLevel" view
    H.br

    H.p $ H.strong "Announcements for future MuniHacs"
    DH.label "keepMePosted" view $
        "Should we send you an email announcing next year's MuniHac?"
    DH.inputCheckbox "keepMePosted" view
