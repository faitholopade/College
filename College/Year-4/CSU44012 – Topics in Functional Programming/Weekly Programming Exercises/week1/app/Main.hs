module Main (main) where

data Tree a = Leaf a
            | Node (Tree a) (Tree a)
            deriving Show

-- Function to count the number of leaf nodes in the tree
count :: Tree a -> Integer
count (Leaf _) = 1
count (Node left right) = count left + count right

-- Function to determine the depth of the tree
depth :: Tree a -> Integer
depth (Leaf _) = 1
depth (Node left right) = 1 + max (depth left) (depth right)

-- Function to flatten the tree into a list (pre-order traversal)
flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node left right) = flatten left ++ flatten right

-- Example Tree
exampleTree :: Tree Int
exampleTree = Node (Leaf 1) (Node (Leaf 2) (Leaf 3))

-- Main function to demonstrate the results
main :: IO ()
main = do
    putStrLn $ "Tree: " ++ show exampleTree
    putStrLn $ "Number of nodes: " ++ show (count exampleTree)
    putStrLn $ "Depth of the tree: " ++ show (depth exampleTree)
    putStrLn $ "Flattened tree: " ++ show (flatten exampleTree)
