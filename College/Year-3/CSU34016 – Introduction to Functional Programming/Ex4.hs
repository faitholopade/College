module Ex4 where

--required for Q1
data Xprsn -- the expression datatype
  = Literal Float -- floating-point value
  | VName String -- variable/identifier name
  | DvdBy Xprsn Xprsn -- divide first by second
  | Minus Xprsn Xprsn -- subtracts second from first
  | Negate Xprsn -- numerical negation (-x)
  -- the following are boolean expressions (using numbers)
  -- the number 0.0 represents False, all others represent True.
  | Not Xprsn -- logical not
  | Eql Xprsn Xprsn -- True if both are the same
  | NotZero Xprsn -- True if numeric value is non-zero
  deriving (Eq,Ord,Show)

type Dict = [(String,Float)]
insert :: String -> Float -> Dict -> Dict
insert s f d = (s,f):d
find :: MonadFail m => String -> Dict -> m Float
find s [] = fail (s++" not found")
find s ((t,f):d)
  | s==t       =  return f
  | otherwise  =  find s d

-- required for Q2
x `incfst` _  =  x + 1
_ `incsnd` y  =  1 + y
type Thing = ([Float],Int)

-- required for all Qs:

-- DON'T RENAME THE SPECIFIED TYPES OR FUNCTIONS
-- DON'T MODIFY ANYTHING ABOVE THIS LINE

-- Q1 (8 marks)
-- implement the following function (which always returns a value):
mdeval :: MonadFail m => Dict -> Xprsn -> m Float
mdeval d (Literal x)   = return x
mdeval d (VName s)     = find s d
mdeval d (DvdBy x y)   = do
    x' <- mdeval d x
    y' <- mdeval d y
    if y' == 0 then fail "Divide by zero" else return (x' / y')
mdeval d (Minus x y)   = do
    x' <- mdeval d x
    y' <- mdeval d y
    return (x' - y')
mdeval d (Negate x)    = fmap negate (mdeval d x)
mdeval d (Not x)       = fmap (\x -> if x == 0.0 then 1.0 else 0.0) (mdeval d x)
mdeval d (Eql x y)     = do
    x' <- mdeval d x
    y' <- mdeval d y
    return $ if x' == y' then 1.0 else 0.0
mdeval d (NotZero x)   = fmap (\x -> if x /= 0.0 then 1.0 else 0.0) (mdeval d x)

-- Q2 (8 marks)
-- Consider the following four recursive pattern definitions:
len :: Int -> [Int] -> Int
len z []     = z
len z (x:xs) = len (z `incfst` x) xs
sumup :: Int -> [Int] -> Int
sumup sbase []     = sbase
sumup sbase (n:ns) = sumup (sbase + n) ns
prod :: Int -> [Int] -> Int
prod mbase []     = mbase
prod mbase (n:ns) = prod (mbase * n) ns
cat :: [Thing] -> [[Thing]] -> [Thing]
cat pfx []     = pfx
cat pfx (xs:xss) = cat (pfx ++ xs) xss

-- They all have the same abstract pattern,
-- as captured by the following Higher Order Function (HOF):
foldL z _ [] = z
foldL z op (x:xs) = foldL (z `op` x) op xs

-- We can gather the `z` and `opr` arguments into a tuple: (op,z)
-- which allows us to construct a call to foldL as:
dofold (op,z) = foldL z op

-- Your task is to complete the tuples below,
-- so that `dofold` can be used to implement the fns. above.

-- dofold lenTuple = len
lenTuple :: (Int -> Int -> Int,Int)
lenTuple = (incfst, 0)

-- dofold sumupTuple = sumup
sumupTuple :: (Int -> Int -> Int,Int)
sumupTuple = ((+), 0)

-- dofold prodTuple = prod
prodTuple :: (Int -> Int -> Int,Int)
prodTuple = ((*), 1)

-- dofold catTuple = cat
catTuple :: ([Thing] -> [Thing] -> [Thing],[Thing])
catTuple = ((++), [])

-- Q3 (11 marks)
sub = subtract -- shorter!
ops = [(27-),(sub 21),(sub 27),(31-),(sub 21),(*29),(sub 31),(+20),(+25),(+24)]

-- (!) This question requires modifying Main.hs
-- See, and/or compile and run Main.hs for further details

-- add extra material below here
-- e.g.,  helper functions, test values, etc. ...

-- Testing function Q1
testMdeval :: IO ()
testMdeval = do
    let dict = [("x", 5.0), ("y", 3.0)]
    putStrLn $ "Literal Test: " ++ show (mdeval dict (Literal 10) == Just 10)
    putStrLn $ "VName Test: " ++ show (mdeval dict (VName "x") == Just 5.0)
    putStrLn $ "DvdBy Test: " ++ show (mdeval dict (DvdBy (VName "x") (VName "y")) == Just (5.0 / 3.0))
    putStrLn $ "Minus Test: " ++ show (mdeval dict (Minus (VName "x") (VName "y")) == Just (5.0 - 3.0))
    putStrLn $ "Negate Test: " ++ show (mdeval dict (Negate (VName "x")) == Just (-5.0))
    putStrLn $ "Not Test: " ++ show (mdeval dict (Not (Literal 0)) == Just 1.0)
    putStrLn $ "Eql Test: " ++ show (mdeval dict (Eql (Literal 5) (VName "x")) == Just 1.0)
    putStrLn $ "NotZero Test: " ++ show (mdeval dict (NotZero (VName "y")) == Just 1.0)
    putStrLn $ "Division by Zero Test: " ++ show (mdeval dict (DvdBy (Literal 10) (Literal 0)) == Nothing)

-- Testing function Q2
testDofoldTuples :: IO ()
testDofoldTuples = do
    let testListInt = [1, 2, 3, 4, 5]
    let testListThing = [([1.0, 2.0], 1), ([3.0, 4.0], 2)]

    putStrLn $ "Len Tuple Test: " ++ show (dofold lenTuple testListInt == len 0 testListInt)
    putStrLn $ "Sumup Tuple Test: " ++ show (dofold sumupTuple testListInt == sumup 0 testListInt)
    putStrLn $ "Prod Tuple Test: " ++ show (dofold prodTuple testListInt == prod 1 testListInt)
    putStrLn $ "Cat Tuple Test: " ++ show (dofold catTuple [testListThing, testListThing] == cat [] [testListThing, testListThing])