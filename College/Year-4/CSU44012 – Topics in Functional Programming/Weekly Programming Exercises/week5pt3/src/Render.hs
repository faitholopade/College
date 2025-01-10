module Render(Window,defaultWindow,render) where
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
defaultWindow = Window (point (-1.5) (-1.5)) (point 1.5 1.5) (500,500)


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

-- -- render a drawing into an image, then save into a file
-- -- NB: the lookup1 function is a VERY inefficient way to convert screen coordinates to drawing
-- --     coordinates! It should be possible to do this in O(1) time, not O(N) time!!
-- --     If you enlarge the viewport in defaultWindow from 50x50 to 500x500 then you will see the problem.
-- render :: String -> Window -> Drawing -> IO ()
-- render path win sh = writePng path $ generateImage pixRenderer w h
--     where
--       Window _ _ (w,h) = win

--       pixRenderer x y = PixelRGB8 c c c where c = colorForImage $ mapPoint win (x,y)

--       mapPoint :: Window -> (Int,Int) -> Point
--       mapPoint _ p = lookup1 p locations

--       lookup1 :: (Int,Int) -> [((Int,Int), Point)] -> Point
--       lookup1 a m = case lookup a m of
--                       Nothing -> point 0 0
--                       Just x  -> x

--       locations :: [ ((Int,Int), Point) ]
--       locations = concat $ zipWith zip (coords win) (pixels win)
--       colorForImage p | p `inside` sh = 255
--                       | otherwise     = 0



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