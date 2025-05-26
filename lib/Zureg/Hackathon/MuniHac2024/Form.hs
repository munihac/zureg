{-# LANGUAGE OverloadedStrings #-}
module Zureg.Hackathon.MuniHac2024.Form
    ( additionalInfoForm
    , additionalInfoView
    ) where

import qualified Data.Text                         as T
import qualified Text.Blaze.Html5                  as H
import qualified Text.Blaze.Html5.Attributes       as A
import qualified Text.Digestive                    as D
import qualified Text.Digestive.Blaze.Html5        as DH
import           Zureg.Hackathon.MuniHac2024.Model as MH24

additionalInfoForm :: Monad m => D.Form H.Html m MH24.RegisterInfo
additionalInfoForm = RegisterInfo
    <$> ("tshirt" D..: (D.validate tshirtCheck $ (,)
            <$> "cut" D..: D.choice
                    [ (Nothing,     "I don't want a T-Shirt")
                    , (Just Female, "Female")
                    , (Just Male,   "Male")
                    ] (Just (Just Male))
            <*> "size" D..: D.choice (
                    [(Just s, H.toHtml $ show s) | s <- [minBound .. maxBound]] ++
                    [(Nothing, "I don't want a T-Shirt")])
                    (Just (Just M))))
    <*> "foodPreference" D..: D.choice
            [ (Nothing, "None")
            , (Just Vegetarian, "I prefer vegetarian food")
            , (Just Vegan, "I prefer vegan food")
            ]
            (Just Nothing)
    <*> "expertiseLevel" D..: D.choice
            [ (Nothing, "I'd rather not say")
            , (Just Beginner, "I've just started learning Haskell")
            , (Just Advanced, "I know my way around Haskell") ]
            (Just Nothing)
    <*> "askMeAbout" D..: optionalText
    <*> "region" D..: D.choice (
            (Nothing, "I'd rather not say") :
            [(Just s, H.toHtml $ show s) | s <- [minBound .. maxBound]])
            (Just Nothing)
    <*> ("project" D..: (Project
            <$> "name" D..: optionalText
            <*> "website" D..: optionalText
            <*> "description" D..: optionalText
            <*> ("contributorLevel" D..: (ContributorLevel
                    <$> "beginner" D..: D.bool Nothing
                    <*> "intermediate" D..: D.bool Nothing
                    <*> "advanced" D..: D.bool Nothing))))
    <*> "keepMePosted" D..: D.bool (Just False)
  where
    tshirtCheck (Just c,  Just s)  = D.Success . Just $ TShirtInfo c s
    tshirtCheck (Nothing, Nothing) = D.Success Nothing
    tshirtCheck (_,       _)       = D.Error
        "Fill in both T-Shirt cut and size or neither of the two"
    optionalText =
        (\t -> let t' = T.strip t in if T.null t' then Nothing else Just t') <$>
        (D.text Nothing)

additionalInfoView :: D.View H.Html -> H.Html
additionalInfoView view = do
    H.h2 "Optional information"

    H.p $ H.strong "T-Shirt"
    H.p $ "If you were to get a T-Shirt, what size and cut would it be?"
    DH.label "tshirt.cut" view "Cut"
    DH.inputSelect "tshirt.cut" view
    H.br
    DH.label "tshirt.size" view "Size"
    DH.inputSelect "tshirt.size" view
    H.br

    H.p $ H.strong "Food Preferences"
    DH.inputSelect "foodPreference" view
    H.br

    H.p $ H.strong "Your Level of Expertise"
    DH.label "expertiseLevel" view $
        "Let us know whether you're a Haskell beginner or expert!"
    DH.inputSelect "expertiseLevel" view
    H.br

    DH.label "askMeAbout" view $ H.strong "Ask me about"
    H.p $ do
        "Topic(s) that you want to talk about with others.  It's a good ice "
        "breaker for people who want to chat with you."
    DH.inputText "askMeAbout" view
    H.br

    H.p $ H.strong "Region"
    DH.label "region" view $ do
        "From what area will you attend MuniHac?  This is purely for our "
        "statistics."
    DH.inputSelect "region" view
    H.br

    H.h2 "Project (optional)"
    H.p $ do
        "Do you have a project or an idea to hack on with others? Do you have "
        "something you want to teach people?"
    H.p $ do
        "We greatly appreciate projects. We have had very good experience with "
        "announcing the project early on the homepage, so that potential "
        "participants can prepare before the Hackathon.  Of course, we're also "
        "happy to add projects during the Hackathon itself, so if you're not "
        "sure yet, don't worry about it."
    DH.label "project.name" view "Project name"
    DH.inputText "project.name" view
    DH.label "project.website" view "Project website"
    DH.inputText "project.website" view
    DH.label "project.description" view "Project description"
    DH.inputText "project.description" view
    H.p "Recommended contributor level(s)"
    DH.inputCheckbox "project.contributorLevel.beginner" view H.! A.class_ "checkbox"
    DH.label "project.contributorLevel.beginner" view $ "Beginner"
    H.br
    DH.inputCheckbox "project.contributorLevel.intermediate" view H.! A.class_ "checkbox"
    DH.label "project.contributorLevel.intermediate" view $ "Intermediate"
    H.br
    DH.inputCheckbox "project.contributorLevel.advanced" view H.! A.class_ "checkbox"
    DH.label "project.contributorLevel.advanced" view $ "Advanced"

    H.p $ H.strong "Announcements for future MuniHacs"
    H.p "Should we send you an email announcing next year's MuniHac?"
    DH.inputCheckbox "keepMePosted" view H.! A.class_ "checkbox"
    DH.label "keepMePosted" view "Yes, please!"
