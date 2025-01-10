import Control.Parallel (par, pseq)

quicksort :: (Ord a) => [a] -> [a]
quicksort []     = []
quicksort [x]    = [x]
quicksort (x:xs) =
  leftpartition `par` rightpartition `pseq` (leftpartition ++ (x:rightpartition))
  where leftpartition = quicksort [y | y <- xs, y < x]
        rightpartition = quicksort [y | y <- xs, y >= x]

main :: IO ()
main = print (quicksort [4, 2, 7, 1, 9])
