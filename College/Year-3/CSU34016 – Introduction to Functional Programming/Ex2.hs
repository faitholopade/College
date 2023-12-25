module Ex2 where

add :: Int -> Int -> Int
add x y = (x+y) `mod` 65563

mul :: Int -> Int -> Int
mul x y
  | p == 0    = 1
  | otherwise = p
  where p = (x*y) `mod` 65563

-- DON'T RENAME THE SPECIFIED FUNCTIONS (f1..fN)
-- DON'T MODIFY ANYTHING ABOVE THIS LINE

-- Q1 (3 marks)
f1 :: [a] -> [a]
-- returns a list of every 139th element of its input
f1 xs = map snd $ filter (\(i, _) -> (i+1) `mod` 139 == 0) $ zip [0..] xs

-- Q2 (3 marks)
f2 :: [Int] -> Int
-- sums every 250th element of its input
f2 ns = sum $ map snd $ filter (\(i, _) -> (i+1) `mod` 250 == 0) $ zip [0..] ns

-- Q3 (4 marks)
f3 :: [Int] -> Int
-- multiplies every 306th element of its input
f3 ns = foldl mul 1 $ map snd $ filter (\(i, _) -> i `mod` 306 == 0) $ zip [1..] ns
  where
    mul a b = a * b

-- Q4 (8 marks)
f4 :: [Maybe Int] -> (Int,[Maybe Int])
f4 [] = (0, [])
f4 (mi@(Just x):mis) = perform x (mis)
f4 (Nothing:mis) = f4 mis -- skip Nothing if it's not an operand

-- Operation Table (See Exercise2 description on BB)
--    ___________________________________________
--    | opcode | operation | operands | Nothing |
--    -------------------------------------------
--    |   52   |    add    | fixed 4  | term    |
--    |   31   |    add    | fixed 4  | skip    |
--    |   40   |    add    | fixed 6  | 6       |
--    |   29   |    add    | stop@ 5  | term    |
--    |   26   |    add    | stop@ 6  | skip    |
--    |   23   |    add    | stop@ 5  | 5       |
--    |   67   |    mul    | fixed 5  | term    |
--    |   20   |    mul    | fixed 3  | skip    |
--    |   37   |    mul    | fixed 3  | 4       |
--    |   63   |    mul    | stop@ 3  | term    |
--    |   12   |    mul    | stop@ 5  | skip    |
--    |   71   |    mul    | stop@ 3  | 2       |
--    -------------------------------------------
-- f4 mis = undefined

-- Q5 (2 marks)
f5 :: [Maybe Int] -> [Int]
f5 mis = go mis
    where
        go [] = []
        go xs =
            let (result, rest) = f4 xs
            in if result == 0 then []
                else if null rest then [result]
                else result : go rest
-- uses f4 to process all the opcodes in the maybe list,
-- by repeatedly applying it to the leftover part
-- f5 mis = undefined

-- add extra material below here
-- e.g.,  helper functions, test values, etc. ...
-- Helper function to perform operations based on opcodes
-- The function returns the result and the remaining list after operation
perform :: Int -> [Maybe Int] -> (Int, [Maybe Int])
perform opcode mis = 
    case opcode of
        52 -> addOp 4 term mis
        31 -> addOp 4 skip mis
        40 -> addOp 6 (const 6) mis
        29 -> addOpUntil 5 term mis
        26 -> addOpUntil 6 skip mis
        23 -> addOpUntil 5 (const 5) mis
        67 -> mulOp 5 term mis
        20 -> mulOp 3 skip mis
        37 -> mulOp 3 (const 4) mis
        63 -> mulOpUntil 3 term mis
        12 -> mulOpUntil 5 skip mis
        71 -> mulOpUntil 3 (const 2) mis
        _  -> (0, Just opcode:mis) -- default case for non-opcod
-- include non-opcode in the returned list

-- The 'addOp' function calculates the sum for 'fixed N' operations
addOp :: Int -> (Maybe Int -> Int) -> [Maybe Int] -> (Int, [Maybe Int])
addOp n nothingHandler mis =
    let (operands, rest) = splitAt n mis
        result = foldl (\acc mi -> add acc (maybe (nothingHandler mi) id mi)) 0 operands
    in (result, rest)

-- The 'mulOp' function calculates the product for 'fixed N' operations
mulOp :: Int -> (Maybe Int -> Int) -> [Maybe Int] -> (Int, [Maybe Int])
mulOp n nothingHandler mis =
    let (operands, rest) = splitAt n mis
        result = foldl (\acc mi -> mul acc (maybe (nothingHandler mi) id mi)) 1 operands
    in (result, rest)

-- The 'addOpUntil' function calculates the sum until a stop value is encountered
addOpUntil :: Int -> (Maybe Int -> Int) -> [Maybe Int] -> (Int, [Maybe Int])
addOpUntil stopValue nothingHandler = go 0
    where
        go acc [] = (acc, [])
        go acc (mi@(Just x):mis)
            | x == stopValue = (acc, mis)
            | otherwise = go (add acc x) mis
        go acc (Nothing:mis) = go (add acc (nothingHandler Nothing)) mis

-- The 'mulOpUntil' function calculates the product until a stop value is encountered
mulOpUntil :: Int -> (Maybe Int -> Int) -> [Maybe Int] -> (Int, [Maybe Int])
mulOpUntil stopValue nothingHandler = go 1
    where
        go acc [] = (acc, [])
        go acc (mi@(Just x):mis)
            | x == stopValue = (acc, mis)
            | otherwise = go (mul acc x) mis
        go acc (Nothing:mis) = go (mul acc (nothingHandler Nothing)) mis

-- Handlers for Nothing values based on opcode
term :: Maybe Int -> Int
term _ = 0

skip :: Maybe Int -> Int
skip Nothing = 0
skip (Just x) = x


