module Shapes where

import Data.List (sortOn)
import Data.Word

type Point = Vector
type Drawing = [(Transform, Shape)]
type ColoredDrawing = [(Transform, Shape, Color)]

data Shape = Circle
           | Rectangle Double Double
           | Ellipse Double Double
           | Polygon [Point]
           deriving Show

-- Define basic shapes
circle :: Shape
circle = Circle

rectangle :: Double -> Double -> Shape
rectangle width height = Rectangle width height

ellipse :: Double -> Double -> Shape
ellipse radiusX radiusY = Ellipse radiusX radiusY

-- Polygon is convex and points are ordered clockwise
polygon :: [Point] -> Shape
polygon points
    | isConvex points = Polygon (orderClockwise points)
    | otherwise = error "Polygon is not convex"

-- Check if polygon is convex
isConvex :: [Point] -> Bool
isConvex (p1:p2:p3:ps) = all (>= 0) crossProducts || all (<= 0) crossProducts
  where
    crossProducts = zipWith3 crossProduct (p1:p2:p3:ps) (p2:p3:ps ++ [p1]) (p3:ps ++ [p1, p2])
    crossProduct (Vector x1 y1) (Vector x2 y2) (Vector x3 y3) =
      (x2 - x1) * (y3 - y2) - (y2 - y1) * (x3 - x2)  -- Cross product formula
isConvex _ = True  -- Polygons with fewer than 3 points are trivially convex

-- Order points in clockwise direction based on their angle relative to the centroid
orderClockwise :: [Point] -> [Point]
orderClockwise points = sortOn (angleToCentroid centroid) points
  where
    centroid = Vector (avg getX points) (avg getY points)
    avg f xs = sum (map f xs) / fromIntegral (length xs)
    angleToCentroid (Vector cx cy) (Vector x y) = atan2 (y - cy) (x - cx)

-- Transformations
data Transform = Identity
               | Translate Vector
               | Scale Vector
               | Rotate Double
               | Shear Double Double
               | Compose Transform Transform
               deriving Show

translate :: Vector -> Transform
translate = Translate

scale :: Vector -> Transform
scale = Scale

rotate :: Double -> Transform
rotate = Rotate

shear :: Double -> Double -> Transform
shear xFactor yFactor = Shear xFactor yFactor

(<+>) :: Transform -> Transform -> Transform
t1 <+> t2 = Compose t1 t2

applyTransform :: Transform -> Point -> Point
applyTransform Identity p = p
applyTransform (Translate (Vector tx ty)) (Vector x y) = Vector (x + tx) (y + ty)
applyTransform (Scale (Vector sx sy)) (Vector x y) = Vector (x * sx) (y * sy)
applyTransform (Rotate angle) (Vector x y) = Vector (x * cos angle - y * sin angle) (x * sin angle + y * cos angle)
applyTransform (Shear sx sy) (Vector x y) = Vector (x + sx * y) (y + sy * x)
applyTransform (Compose t1 t2) p = applyTransform t2 (applyTransform t1 p)

-- Relative positioning functions
over :: ColoredDrawing -> ColoredDrawing -> ColoredDrawing
over top bottom = bottom ++ top

left :: ColoredDrawing -> ColoredDrawing -> ColoredDrawing
left first second = first ++ [(translate (point (getDrawingWidth first + 1) 0), shape, color) | (Identity, shape, color) <- second]

right :: ColoredDrawing -> ColoredDrawing -> ColoredDrawing
right first second = left second first

above :: ColoredDrawing -> ColoredDrawing -> ColoredDrawing
above first second = first ++ [(translate (point 0 (getDrawingHeight first + 1)), shape, color) | (Identity, shape, color) <- second]

below :: ColoredDrawing -> ColoredDrawing -> ColoredDrawing
below first second = above second first

-- Helper functions to calculate the width and height of a drawing
getDrawingWidth :: ColoredDrawing -> Double
getDrawingWidth drawing = maximum $ map (getShapeWidth . snd3) drawing

getDrawingHeight :: ColoredDrawing -> Double
getDrawingHeight drawing = maximum $ map (getShapeHeight . snd3) drawing

getShapeWidth :: Shape -> Double
getShapeWidth Circle = 2
getShapeWidth (Rectangle w _) = w
getShapeWidth (Ellipse rx _) = 2 * rx
getShapeWidth (Polygon points) = maximum (map getX points) - minimum (map getX points)

getShapeHeight :: Shape -> Double
getShapeHeight Circle = 2
getShapeHeight (Rectangle _ h) = h
getShapeHeight (Ellipse _ ry) = 2 * ry
getShapeHeight (Polygon points) = maximum (map getY points) - minimum (map getY points)

-- Helper to extract second item from a tuple
snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

-- Vector for 2D points
data Vector = Vector Double Double deriving Show

point :: Double -> Double -> Vector
point = Vector

getX :: Vector -> Double
getX (Vector x _) = x

getY :: Vector -> Double
getY (Vector _ y) = y

-- Color data type
data Color = Color { r :: Word8, g :: Word8, b :: Word8, a :: Word8 } deriving Show

createColor :: Word8 -> Word8 -> Word8 -> Word8 -> Color
createColor = Color

transparent :: Color
transparent = createColor 0 0 0 0

-- Predefined colors
red, green, blue :: Color
red = createColor 255 0 0 255
green = createColor 0 255 0 255
blue = createColor 0 0 255 255

-- Sample drawings demonstrating the required functionality:

-- Circle with scaling and translation
circleDrawing :: ColoredDrawing
circleDrawing = [(scale (point 3 3) <+> translate (point (-1) 1), circle, red)]

-- Rectangle with identity transform
rectangleDrawing :: ColoredDrawing
rectangleDrawing = [(Identity, rectangle 4 2, blue)]

-- Convex polygon with clockwise ordered points
polygonDrawing :: ColoredDrawing
polygonDrawing = [(Identity, polygon [point (-1) 0, point 1 0, point 0 1], green)]

-- Ellipse with rotation
ellipseDrawing :: ColoredDrawing
ellipseDrawing = [(rotate 45, ellipse 2 1, red)]

-- Example showing shear transformation on a rectangle
shearedRectangleDrawing :: ColoredDrawing
shearedRectangleDrawing = [(shear 0.5 0, rectangle 4 2, blue)]

-- Example combining two shapes: Circle over Rectangle
circleOverRectangle :: ColoredDrawing
circleOverRectangle = circleDrawing `over` rectangleDrawing
