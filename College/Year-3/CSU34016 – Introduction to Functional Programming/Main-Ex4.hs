module Main where

import Ex4
import System.IO

main :: IO ()
main = do
    -- Read the input file
    inputFile <- readFile "input.dat"
    let numbers = map read . lines $ inputFile :: [Integer]

    -- Apply operations
    let results = zipWith ($) (cycle ops) numbers

    -- Write to the output file
    writeFile "output.dat" (unlines . map show $ results)
