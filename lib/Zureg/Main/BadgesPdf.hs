{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Zureg.Main.BadgesPdf (main) where

import           Control.Monad        (guard)
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv             as Csv
import           Data.Foldable        (for_)
import           Data.Maybe           (fromMaybe, mapMaybe, maybeToList)
import qualified Data.Text            as T
import qualified Graphics.Rendering.Cairo as C
import           System.Environment   (getArgs, getProgName)
import           System.Exit          (exitFailure)
import qualified System.IO            as IO
import           Zureg.Hackathon      (Hackathon)
import           Zureg.Model

-- | Converts millimeters to Cairo units (= points)
millimeters :: Double -> Double
millimeters mm = mm / 25.4 * 72

cardWidth, cardHeight, pageWidth, pageHeight :: Double
cardWidth = millimeters 90
cardHeight = millimeters 54
pageWidth = millimeters 210
pageHeight = millimeters 297

createRegistrantPdf :: [Registrant a] -> C.Render ()
createRegistrantPdf [] = pure ()
createRegistrantPdf registrants = do
    renderPage cardsPerPageH cardsPerPageV registrants
    C.showPage
    createRegistrantPdf (drop cardsPerPage registrants)
  where
    cardsPerPageH = floor (pageWidth / cardWidth)
    cardsPerPageV = floor (pageHeight / cardHeight)
    cardsPerPage = cardsPerPageH * cardsPerPageV

renderPage :: Int -> Int -> [Registrant a] -> C.Render ()
renderPage cardsPerPageH cardsPerPageV registrants = do
    C.save
    C.translate leftMargin topMargin
    for_ (distributeOnPage cardsPerPageH cardsPerPageV registrants) $ \row -> do
        C.save
        for_ row $ \reg -> do
            registrantCard reg
            C.translate cardWidth 0
        C.restore
        C.translate 0 cardHeight
    C.restore
  where
    topMargin = (pageHeight - fromIntegral cardsPerPageV * cardHeight) / 2
    leftMargin = (pageWidth - fromIntegral cardsPerPageH * cardWidth) / 2

distributeOnPage :: Int -> Int -> [a] -> [[a]]
distributeOnPage _ _ [] = []
distributeOnPage _ 0 _  = []
distributeOnPage h v xs =
    let (hd, tl) = splitAt h xs
    in  hd : distributeOnPage h (v-1) tl

registrantCard :: Registrant a -> C.Render ()
registrantCard Registrant {..} = do
    C.moveTo 0 0
    C.rectangle 0 0 cardWidth cardHeight
    C.stroke
    showText (fromMaybe mempty (riName <$> rInfo))

showText :: T.Text -> C.Render ()
showText = C.showText . T.unpack

main :: forall a. A.FromJSON a => Hackathon a -> IO ()
main _ = do
    progName <- getProgName
    args     <- getArgs

    case args of
        [exportPath, outputPdf] -> do
            registrantsOrError <- A.eitherDecodeFileStrict exportPath
            registrants <- either (fail . show) return registrantsOrError
                :: IO [Registrant a]

            C.withPDFSurface outputPdf pageWidth pageHeight $ \surface ->
                C.renderWith surface (createRegistrantPdf registrants)

        _ -> do
            IO.hPutStr IO.stderr $ unlines
                [ "Usage: " ++ progName ++ " export.json out.pdf"
                , ""
                , "export.json is a list of registrants as obtained by the"
                , "export tool."
                ]
            exitFailure
