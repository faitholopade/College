module Main where

import Shapes
import Render (render,defaultWindow)

exampleDrawing =  [(scale (point 0.75 0.5) <+> translate (point 0.5 0.5), square)]

main = render "output.png" defaultWindow exampleDrawing
