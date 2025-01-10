module Render where

import Codec.Picture
import Shapes
import Data.Array
import Data.Word

-- Definition of a window for rendering, specified by two corner points and resolution.
data Window = Window Point Point (Int, Int)

-- Default window configuration with specified dimensions and resolution.
defaultWindow :: Window
defaultWindow = Window (point (-10) (-10)) (point 10 10) (500, 500)

-- Calculates the coordinate value for a given pixel index in a range.
sampleCoordinate :: Int -> Double -> Double -> Int -> Double
sampleCoordinate index start end total = start + ((end - start) / fromIntegral (total - 1)) * fromIntegral index

-- Maps pixel coordinates to their corresponding points in the window.
mapPixel :: (Int, Int) -> Window -> Point
mapPixel (pixelX, pixelY) (Window topLeft bottomRight (width, height)) =
    point (sampleCoordinate pixelX (getX topLeft) (getX bottomRight) width)
          (sampleCoordinate (height - pixelY) (getY topLeft) (getY bottomRight) height)

-- Computes an array mapping each pixel to a point in the window.
computePixelMap :: Window -> Array (Int, Int) Point
computePixelMap window@(Window topLeft bottomRight (width, height)) =
  array ((0, 0), (width - 1, height - 1))
        [((x, y), mapPixel (x, y) window) | x <- [0..width-1], y <- [0..height-1]]

-- Function to determine color based on point position and shape.
colorForPoint :: Point -> ColoredDrawing -> Color
colorForPoint p drawing = foldr (\(transform, shape, color) currentColor ->
    if pointInsideShape (applyTransform transform p) shape then color else currentColor) transparent drawing

-- Check if a point is inside a shape.
pointInsideShape :: Point -> Shape -> Bool
pointInsideShape (Vector x y) Circle = x * x + y * y <= 1
pointInsideShape (Vector x y) (Rectangle width height) = abs x <= width / 2 && abs y <= height / 2
pointInsideShape (Vector x y) (Ellipse radiusX radiusY) = (x * x / (radiusX * radiusX)) + (y * y / (radiusY * radiusY)) <= 1
pointInsideShape (Vector px py) (Polygon points) = checkPolygon px py points

-- Helper function to check if a point is inside a polygon (using ray-casting algorithm).
checkPolygon :: Double -> Double -> [Point] -> Bool
checkPolygon px py vertices = odd (length $ filter intersects edges)
  where
    edges = zip vertices (tail vertices ++ [head vertices])
    intersects ((Vector x1 y1), (Vector x2 y2)) =
        ((y1 > py) /= (y2 > py)) && (px < ((x2 - x1) * (py - y1) / (y2 - y1) + x1))

-- Renders a colored drawing to an image file.
rgbRender :: String -> Window -> ColoredDrawing -> IO ()
rgbRender filePath window shape = writePng filePath $ generateImage pixRenderer width height
  where
    Window _ _ (width, height) = window
    pm = computePixelMap window
    generatePoint (x, y) = pm ! (x, y)
    
    -- Pixel renderer for colored drawings.
    pixRenderer x y = toPixelRGBA8 $ colorForPoint (generatePoint (x, y)) shape
    
    -- Converts a Color value to a PixelRGBA8 value.
    toPixelRGBA8 col = PixelRGBA8 (r col) (g col) (b col) (a col)
