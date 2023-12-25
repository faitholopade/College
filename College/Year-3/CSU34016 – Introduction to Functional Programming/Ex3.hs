module Ex3 where

--required for all Qs:
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
find :: String -> Dict -> Maybe Float
find s [] = Nothing
find s ((t,f):d)
  | s==t       =  Just f
  | otherwise  =  find s d

-- DON'T RENAME THE SPECIFIED TYPES OR FUNCTIONS
-- DON'T MODIFY ANYTHING ABOVE THIS LINE

-- Q1 (8 marks)
-- implement the following function (which may have runtime errors):
eval :: Dict -> Xprsn -> Float
eval _ (Literal f) = f
eval d (VName s) = case find s d of
                     Just f -> f
                     Nothing -> error "Variable not found"
eval d (DvdBy x y) = eval d x / eval d y
eval d (Minus x y) = eval d x - eval d y
eval d (Negate x) = - (eval d x)
eval d (Not x) = if eval d x == 0.0 then 1.0 else 0.0
eval d (Eql x y) = if eval d x == eval d y then 1.0 else 0.0
eval d (NotZero x) = if eval d x /= 0.0 then 1.0 else 0.0


-- Q2 (8 marks)
-- implement the following function (which always returns a value):
meval :: Dict -> Xprsn -> Maybe Float
meval _ (Literal f) = Just f
meval d (VName s) = find s d
meval d (DvdBy x y) = case meval d y of
                        Just 0.0 -> Nothing -- Avoid division by zero
                        Just fy -> fmap (/ fy) (meval d x)
                        Nothing -> Nothing
meval d (Minus x y) = case (meval d x, meval d y) of
                        (Just vx, Just vy) -> Just (vx - vy)
                        _ -> Nothing
meval d (Negate x) = fmap negate (meval d x)
meval d (Not x) = fmap (\vx -> if vx == 0.0 then 1.0 else 0.0) (meval d x)
meval d (Eql x y) = case (meval d x, meval d y) of
                        (Just vx, Just vy) -> Just (if vx == vy then 1.0 else 0.0)
                        _ -> Nothing
meval d (NotZero x) = fmap (\vx -> if vx /= 0.0 then 1.0 else 0.0) (meval d x)



-- Q3 (4 marks)
-- Laws of Arithmetic for this question:
--    x + 0 = x
--    0 + x = x
--    x - 0 = x
--    x - x = 0
--    x * 0 = 0
--    1 * x = x
-- The following function should implement the two laws applicable
-- for *your* Xprsn datatype.
simp :: Xprsn -> Xprsn
simp (Minus x (Literal 0.0)) = simp x
simp (Minus x y) | x == y = Literal 0.0
simp (DvdBy x (Literal 1.0)) = simp x
simp x = x -- default case


-- add extra material below here
-- e.g.,  helper functions, test values, etc. ...

-- let d = [("x", 10.0), ("y", 5.0)]
--   eval d (Literal 5) -- Should return 5.0
--   eval d (VName "x") -- Should return 10.0
--   eval d (DvdBy (Literal 10) (Literal 2)) -- Should return 5.0

-- meval d (DvdBy (Literal 10) (Literal 0)) -- Should return Nothing
-- meval d (Minus (VName "x") (VName "y")) -- Should return Just 5.0

-- simp (Minus (Literal 5) (Literal 0)) -- Should return Literal 5
-- simp (DvdBy (Literal 10) (Literal 1)) -- Should return Literal 10




