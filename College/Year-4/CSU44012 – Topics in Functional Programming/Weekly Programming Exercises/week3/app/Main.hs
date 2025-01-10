-- Part 1: Custom List type and its instances
-- ==========================================

-- We define our own List type to avoid clashing with Haskell's built-in list.
-- This List type will support Functor, Applicative, and Monad.
data List a = Nil | Cons a (List a)
    deriving (Show)

-- Functor instance for List:
-- The Functor instance allows us to apply a function to every element in the list.
-- This is done using the fmap function, which traverses the list and applies the function.
instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- Applicative instance for List:
-- Applicative gives us more power. It allows us to apply a list of functions to a list of values.
-- The pure function wraps a single value into a list, and <*> applies functions from one list to values in another.
instance Applicative List where
    pure x = Cons x Nil  -- Wraps a single value into a list.
    Nil <*> _ = Nil      -- If the function list is empty, nothing can be applied.
    _ <*> Nil = Nil      -- If the value list is empty, nothing can be applied.
    (Cons f fs) <*> xs = append (fmap f xs) (fs <*> xs)  -- Apply each function in the list to the values in the other list.

-- Monad instance for List:
-- The Monad instance introduces the bind (>>=) function. This allows us to chain operations, applying a function
-- that returns a new list to each element of the original list, then combining the results.
instance Monad List where
    return = pure  -- return is the same as pure: it creates a list with one element.
    Nil >>= _ = Nil  -- If the list is empty, there’s nothing to bind.
    Cons x xs >>= f = append (f x) (xs >>= f)  -- Apply f to the head (x) and bind it recursively to the tail (xs).

-- Helper function to append two lists:
-- This is used to concatenate two lists, which is useful in both the Applicative and Monad instances.
append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

-- To show that the Monad instance is correct, we can check the three Monad laws:
-- 1. Left Identity: return a >>= f should give the same result as f a.
--    return creates a list with just one element a, and >>= will apply f to it.
--    Example: return 5 >>= f is the same as f 5.
-- 2. Right Identity: m >>= return should give us back the original list m.
--    Since return wraps the element in a list and >>= flattens lists, nothing changes.
-- 3. Associativity: (m >>= f) >>= g should be the same as m >>= (\x -> f x >>= g).
--    This holds true because the recursive structure of the list ensures that chaining operations happens correctly.

-- Testing List Monad:
testList :: List Int
testList = Cons 1 (Cons 2 (Cons 3 Nil))

testFuncs :: List (Int -> Int)
testFuncs = Cons (*2) (Cons (+1) Nil)

-- Function to test Monad bind (>>=):
-- This function will be applied to each element of the list to check the bind operation.
testFunc :: Int -> List Int
testFunc x = Cons (x * 2) (Cons (x + 1) Nil)

-- Part 2: Functor instance for Pair and why Pair can't be an Applicative
-- =====================================================================

-- The Pair type represents a 2-tuple (a, b), where a is the first element and b is the second.
data Pair a b = P a b
    deriving (Show)

-- Functor instance for Pair:
-- Functor works on the second element of the Pair (the b part).
-- The first element (a) is left untouched, as Functor operates on the second type parameter (b).
instance Functor (Pair a) where
    fmap f (P x y) = P x (f y)

-- Functor laws for Pair:
-- 1. Identity: If we apply fmap id to a pair, nothing should change.
--    Example: fmap id (P 1 2) gives P 1 2, which holds.
-- 2. Composition: fmap (f . g) should give the same result as fmap f followed by fmap g.
--    Example: fmap ((+1) . (*2)) (P 1 2) gives the same result as fmap (+1) (fmap (*2) (P 1 2)).

-- Why Pair can't be an Applicative:
-- To implement Applicative, we would need a way to define pure for Pair.
-- The problem is that pure requires us to create a Pair with only a value for the second element (b),
-- but Pair also requires an a. There's no way to generate the first element (a) automatically,
-- so we can't implement pure or <*> properly for Pair. This is why Pair can't be an Applicative.

-- Testing Pair Functor:
testPair :: Pair String Int
testPair = P "Hello" 42

-- Main function to run all the tests:
-- This function will print out the results of testing Functor, Applicative, and Monad instances for List,
-- as well as testing the Functor instance for Pair.
main :: IO ()
main = do
    -- Testing Functor on List:
    putStrLn "Testing Functor (fmap) on List:"
    print (fmap (*2) testList)  -- Expected: Cons 2 (Cons 4 (Cons 6 Nil))
    
    -- Testing Applicative on List:
    putStrLn "Testing Applicative on List:"
    print (testFuncs <*> testList)
    -- Expected: Cons 2 (Cons 4 (Cons 6 (Cons 2 (Cons 3 (Cons 4 Nil)))))

    -- Testing Monad (>>=) on List:
    putStrLn "Testing Monad (>>=) on List:"
    print (testList >>= testFunc)
    -- Expected: Cons 2 (Cons 2 (Cons 4 (Cons 3 (Cons 6 (Cons 4 Nil)))))

    -- Testing Functor on Pair:
    putStrLn "Testing Functor (fmap) on Pair:"
    print (fmap (+1) testPair)
    -- Expected: P "Hello" 43
