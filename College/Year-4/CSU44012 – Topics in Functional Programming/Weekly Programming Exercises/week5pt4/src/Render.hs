module Render where
import Codec.Picture
import Shapes
import Data.Word (Word8) 


--  A window specifies what part of the world to render and at which
--  resolution.
--  Values are top left & bottom right corner to be rendered,
--             and the size of the output device to render into
data Window = Window Point Point (Int,Int)

-- Default window renders a small region around the origin into
-- a 100x100 pixel image
-- Note the comment below on the 'render' function. If you increase
-- the size of the output image to around 500 by 500 you'll see what
-- I mean by inefficient
defaultWindow :: Window
defaultWindow = Window (point (-1.5) (-1.5)) (point 1.5 1.5) (100,100)


-- Generate a list of evenly spaced samples between two values.
-- e.g. samples -1.5 +1.5 25 generates 25 samples evenly spaced
--      between the two bounds: [ -1.5, -1.375, -1.25 ... ]
samples :: Double -> Double -> Int -> [Double]
samples c0 c1 n = take n [ c0, c0 + (c1-c0) / fromIntegral (n-1) .. ]

-- Generate the matrix of points corresponding to the pixels of a window.
pixels :: Window -> [[Point]]
pixels (Window p0 p1 (w,h)) =
  [ [ point x y | x <- samples (getX p0) (getX p1) w ]
                | y <- reverse $ samples (getY p0) (getY p1) h
  ]

-- generate list of all screen coordinates in window
coords :: Window -> [[(Int,Int)]]
coords (Window _ _ (w,h)) = [ [(x,y) | x <- [0..w]] | y <- [0..h] ]

-- Render a drawing into an image, then save into a file.
-- Changed to remove the inefficient lookup and directly calculate the point for each pixel.
render :: String -> Window -> Drawing -> IO ()
render path win sh = writePng path $ generateImage pixRenderer w h
    where
      Window p0 p1 (w, h) = win

      -- Extract coordinates from Points (Vector alias)
      xMin = getX p0
      yMin = getY p0
      xMax = getX p1
      yMax = getY p1

      -- Calculate the point from pixel coordinates
      mapPoint :: (Int, Int) -> Point
      mapPoint (px, py) = point x y
          where
            x = xMin + (xMax - xMin) * fromIntegral px / fromIntegral (w - 1)
            y = yMax + (yMin - yMax) * fromIntegral py / fromIntegral (h - 1)  -- Reverse because graphics coordinates start from top

      pixRenderer :: Int -> Int -> PixelRGB8
      pixRenderer x y = PixelRGB8 c c c
          where
            c = if point `inside` sh then 255 else 0
            point = mapPoint (x, y)

      -- Calculate colors based on whether the point is inside the shape
      colorForImage :: Point -> Word8
      colorForImage p | p `inside` sh = 255
                      | otherwise     = 0

-- Definition of the Mandelbrot set
mandelbrot :: Int -> Double -> Double -> Bool
mandelbrot maxIter cx cy = let
  iter z@(zx, zy) i =
    if i == maxIter then True
    else if zx*zx + zy*zy > 4 then False
    else iter ((zx*zx - zy*zy + cx), (2*zx*zy + cy)) (i + 1)
  in iter (0, 0) 0

-- Generate the Mandelbrot image
generateMandelbrot :: Int -> Int -> Image PixelRGB8
generateMandelbrot width height = generateImage pixelRenderer width height
  where
    pixelRenderer x y =
      if mandelbrot 100 (scaleX x) (scaleY y)
      then PixelRGB8 0 34 52 -- Color for points inside the set
      else PixelRGB8 255 38 94  -- Color for points outside the set
    scaleX x = 3.5 * (fromIntegral x / fromIntegral width) - 2.5
    scaleY y = 2.0 * (fromIntegral y / fromIntegral height) - 1.0

-- Function to save the image
renderMandelbrot :: FilePath -> IO ()
renderMandelbrot path = writePng path $ generateMandelbrot 500 500