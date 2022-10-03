{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Zureg.Main.BadgesPdf (main) where

import           Control.Monad        (guard, when)
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv             as Csv
import           Data.Foldable        (for_)
import           Data.Maybe           (fromJust, fromMaybe, mapMaybe, maybeToList)
import qualified Data.Text            as T
import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.Rendering.Cairo.SVG as C
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
cardGapWidth = millimeters 10
cardGapHeight = millimeters 0
pageWidth = millimeters 210
pageHeight = millimeters 297

createRegistrantPdf :: C.SVG -> [Registrant a] -> C.Render ()
createRegistrantPdf _ [] = pure ()
createRegistrantPdf template registrants = do
    renderPage Front cardsPerPageH cardsPerPageV template registrants
    C.showPage
    renderPage Back cardsPerPageH cardsPerPageV template registrants
    C.showPage
    createRegistrantPdf template (drop cardsPerPage registrants)
  where
    cardsPerPageH = floor (pageWidth / (cardWidth + cardGapWidth))
    cardsPerPageV = floor (pageHeight / (cardHeight + cardGapHeight))
    cardsPerPage = cardsPerPageH * cardsPerPageV

data PageSide = Front | Back

renderPage :: PageSide -> Int -> Int -> C.SVG -> [Registrant a] -> C.Render ()
renderPage side cardsPerPageH cardsPerPageV template registrants = do
    C.save
    C.translate leftMargin topMargin
    for_ (distributeOnPage cardsPerPageH cardsPerPageV registrants) $ \row -> do
        C.save
        for_ (zip row [0..]) $ \(reg, col) -> do
            C.save
            let xPos = case side of
                    Front -> (cardWidth + cardGapWidth) * fromIntegral col
                    Back -> (cardWidth + cardGapWidth) * fromIntegral (cardsPerPageH - col - 1)
            C.translate xPos 0
            registrantCard template reg
            C.restore
        C.restore
        C.translate 0 (cardHeight + cardGapHeight)
    C.restore
  where
    topMargin = (pageHeight - fromIntegral cardsPerPageV * cardHeight - fromIntegral (cardsPerPageV - 1) * cardGapHeight) / 2
    leftMargin = (pageWidth - fromIntegral cardsPerPageH * cardWidth - fromIntegral (cardsPerPageH - 1) * cardGapWidth) / 2

distributeOnPage :: Int -> Int -> [a] -> [[a]]
distributeOnPage _ _ [] = []
distributeOnPage _ 0 _  = []
distributeOnPage h v xs =
    let (hd, tl) = splitAt h xs
    in  hd : distributeOnPage h (v-1) tl

registrantCard :: C.SVG -> Registrant a -> C.Render ()
registrantCard template Registrant {..} = do
    C.moveTo 0 0
    C.svgRender template
    let registrantInfo = fromJust rInfo
        badgeName = fromMaybe (riName registrantInfo) (riBadgeName registrantInfo)
        affiliation = riAffiliation registrantInfo

    C.moveTo (cardWidth - 12) 36
    showTextAligned 24 (cardWidth - 24) badgeName
    case affiliation of
        Nothing -> pure ()
        Just a -> do
            C.moveTo (cardWidth - 12) 72
            showTextAligned 15 (cardWidth - 72) a

showTextAligned :: Double -> Double -> T.Text -> C.Render ()
showTextAligned fontSize maxWidth txt = do
    C.save
    C.setFontSize fontSize
    C.TextExtents {..} <- C.textExtents txt
    when (textExtentsWidth > maxWidth) $
        C.setFontSize (fontSize * maxWidth / textExtentsWidth)
    C.TextExtents {..} <- C.textExtents txt
    C.relMoveTo (- textExtentsWidth) 0
    C.showText txt
    C.newPath
    C.restore

main :: forall a. A.FromJSON a => Hackathon a -> IO ()
main _ = do
    progName <- getProgName
    args     <- getArgs

    case args of
        [exportPath, templateFile, outputPdf] -> do
            registrantsOrError <- A.eitherDecodeFileStrict exportPath
            registrants <- either (fail . show) return registrantsOrError
                :: IO [Registrant a]

            template <- C.svgNewFromFile templateFile

            C.withPDFSurface outputPdf pageWidth pageHeight $ \surface ->
                C.renderWith surface (createRegistrantPdf template registrants)

        _ -> do
            IO.hPutStr IO.stderr $ unlines
                [ "Usage: " ++ progName ++ " export.json template.svg out.pdf"
                , ""
                , "export.json is a list of registrants as obtained by the"
                , "export tool."
                ]
            exitFailure
