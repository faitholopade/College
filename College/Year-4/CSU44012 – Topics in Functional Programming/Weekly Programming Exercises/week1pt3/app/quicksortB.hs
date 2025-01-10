import Control.Parallel
import Control.Parallel.Strategies

quicksort :: (Ord a) => [a] -> [a]
quicksort []     = []
quicksort [x]    = [x]
quicksort (x:xs) =
  leftpartition `par` rightpartition `pseq` (forceList leftpartition `seq` forceList rightpartition `seq` (leftpartition ++ (x:rightpartition)))
  where leftpartition = quicksort [y | y <- xs, y < x]
        rightpartition = quicksort [y | y <- xs, y >= x]

forceList :: [a] -> ()
forceList [] = ()
forceList (x:xs) = x `seq` forceList xs

main :: IO ()
main = print (quicksort [4, 2, 7, 1, 9])
