{-# LANGUAGE OverloadedStrings #-}

import Data.Text.Lazy
import qualified Web.Scotty as Scotty
import Text.Blaze.Html5 hiding (main)
import Text.Blaze.Html5.Attributes hiding (id)
import Text.Blaze.Html.Renderer.Text
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Shapes
import Render

main :: IO ()
main = do
    -- Render the required images
    rgbRender "circleDrawing.png" defaultWindow circleDrawing
    rgbRender "rectangleDrawing.png" defaultWindow rectangleDrawing
    rgbRender "polygonDrawing.png" defaultWindow polygonDrawing
    rgbRender "ellipseDrawing.png" defaultWindow ellipseDrawing
    rgbRender "shearedRectangle.png" defaultWindow shearedRectangleDrawing
    rgbRender "circleOverRectangle.png" defaultWindow circleOverRectangle
    rgbRender "leftShapes.png" defaultWindow leftShapes
    rgbRender "aboveShapes.png" defaultWindow aboveShapes

    -- Start the Scotty server
    Scotty.scotty 3000 $ do
        Scotty.get "/" $ Scotty.html response
        Scotty.get "/images/:image" $ do
            image <- Scotty.param "image"
            Scotty.file ("./" ++ image ++ ".png")

-- HTML response for the web application
response :: Text
response = renderHtml $ do
    H.head $ H.title "eDSL Drawing Web Application"
    H.body $ do
        H.h1 "Welcome to the eDSL Drawing Web Application"
        H.p "Choose an image to view and see the DSL code that produced it:"
        H.ul $ do
            H.li $ H.a ! A.href "/images/circleDrawing" $ "Circle Drawing"
            H.p "DSL Code:"
            H.pre $ H.code $ toHtml (show circleDSL)
            H.li $ H.a ! A.href "/images/rectangleDrawing" $ "Rectangle Drawing"
            H.p "DSL Code:"
            H.pre $ H.code $ toHtml (show rectangleDSL)
            H.li $ H.a ! A.href "/images/polygonDrawing" $ "Polygon Drawing"
            H.p "DSL Code:"
            H.pre $ H.code $ toHtml (show polygonDSL)
            H.li $ H.a ! A.href "/images/ellipseDrawing" $ "Ellipse Drawing"
            H.p "DSL Code:"
            H.pre $ H.code $ toHtml (show ellipseDSL)
            H.li $ H.a ! A.href "/images/shearedRectangle" $ "Sheared Rectangle"
            H.p "DSL Code:"
            H.pre $ H.code $ toHtml (show shearedRectangleDSL)
            H.li $ H.a ! A.href "/images/circleOverRectangle" $ "Circle Over Rectangle"
            H.p "DSL Code:"
            H.pre $ H.code $ toHtml (show circleOverRectangleDSL)
            H.li $ H.a ! A.href "/images/leftShapes" $ "Left Shapes"
            H.p "DSL Code:"
            H.pre $ H.code $ toHtml (show leftShapesDSL)
            H.li $ H.a ! A.href "/images/aboveShapes" $ "Above Shapes"
            H.p "DSL Code:"
            H.pre $ H.code $ toHtml (show aboveShapesDSL)

-- DSL code for each drawing as text
circleDSL :: String
circleDSL = "[(scale (point 3 3) <+> translate (point (-1) 1), circle, red)]"

rectangleDSL :: String
rectangleDSL = "[(Identity, rectangle 4 2, blue)]"

polygonDSL :: String
polygonDSL = "[(Identity, polygon [point (-1) 0, point 1 0, point 0 1], green)]"

ellipseDSL :: String
ellipseDSL = "[(rotate 45, ellipse 2 1, red)]"

shearedRectangleDSL :: String
shearedRectangleDSL = "[(shear 0.5 0, rectangle 4 2, blue)]"

circleOverRectangleDSL :: String
circleOverRectangleDSL = "circleDrawing `over` rectangleDrawing"

leftShapesDSL :: String
leftShapesDSL = "circleDrawing `left` rectangleDrawing"

aboveShapesDSL :: String
aboveShapesDSL = "circleDrawing `above` rectangleDrawing"

-- New samples demonstrating relative positioning
leftShapes :: ColoredDrawing
leftShapes = circleDrawing `left` rectangleDrawing

aboveShapes :: ColoredDrawing
aboveShapes = circleDrawing `above` rectangleDrawing
