module Main where

-- Question 1: Define a function `f1` that performs all the IO actions in a list in the order provided.
-- This function goes through each IO action and executes them sequentially. 
-- It returns the result of the last IO action.
-- Answer to Question 1:
f1 :: [IO a] -> IO a
f1 [x] = x                -- If there's only one IO action, just return it.
f1 (x:xs) = x >> f1 xs     -- Use the (>>) operator to sequence IO actions.
f1 [] = error "Empty list" -- Return an error for an empty list.

-- Explanation: `f1` runs all the actions in the list sequentially, discarding intermediate results.
-- The result of the last action in the list is returned.
-- For example, if you run `f1 actions` where `actions` is a list of `putChar`, it prints characters in order.

-- Question 2: What should this print?
-- main = f1 actions
-- Answer: This will print "hello" because the `actions` list contains `putChar` operations that print 'h', 'e', 'l', 'l', 'o'.

-- Question 3: What does this print and why?
-- main = do
--        let a = f1 actions
--            b = if True then putChar 'a' else putChar 'b'
--        putStr "Hi there"
-- Answer: This prints "Hi therea".
-- Explanation: 
-- - First, `putStr "Hi there"` prints "Hi there".
-- - Then, the `if True` statement ensures that `putChar 'a'` is executed, appending 'a'.
-- - The `f1 actions` result is evaluated later, and prints "hello" if executed afterward.
-- However, since the structure of the program places `putStr` and `b` first, "Hi therea" is printed.

-- Question 4: Define a new control structure `while` with this signature: `while :: IO Bool -> IO ()`.
-- This structure repeatedly performs the provided action until the action returns False.
-- Answer to Question 4:
while :: IO Bool -> IO ()
while action = do
  result <- action    -- Perform the action and check the result.
  if result then while action else return ()  -- If the result is True, repeat the action; otherwise, stop.

-- Explanation: The `while` function mimics a traditional "while loop" but in the context of IO.
-- It keeps repeating the IO action as long as it returns `True`. When it returns `False`, the loop terminates.

-- Example of `while` usage:
exampleWhile :: IO ()
exampleWhile = while (putStr "Enter 'y' to continue: " >> (== 'y') <$> getChar)

-- Question 5: Define `f2` which takes a list of IO actions and returns a list of their results.
-- Answer to Question 5:
f2 :: [IO a] -> IO [a]
f2 = sequence  -- Use `sequence`, which runs each IO action and gathers their results in a list.

-- Explanation: The `f2` function executes a list of IO actions and collects their results in a list.
-- For example, `f2 [getChar, getChar]` will ask the user for two characters and return them as a list.

-- Example usage for reading 10 characters:
read10 :: IO String
read10 = f2 $ take 10 (repeat getChar)  -- Repeats `getChar` 10 times, collecting the characters into a list.

-- Main function to demonstrate usage of `f1`, `f2`, and `while`
main :: IO ()
main = do
  -- Question 1 & 2 Example: Running `f1` to print "hello"
  putStrLn "Running f1 to print 'hello':"
  f1 actions
  putStrLn ""

  -- Question 3 Example: Running the main block with conditional logic
  let a = f1 actions
      b = if True then putChar 'a' else putChar 'b'
  putStrLn "Running example from question 3:"
  putStr "Hi there"
  b
  putStrLn ""

  -- Question 4 Example: Using `while` to continue asking for input
  putStrLn "Running while loop (enter 'y' to continue):"
  exampleWhile

  -- Question 5 Example: Using `f2` to read 10 characters
  putStrLn "Reading 10 characters (input required):"
  result <- read10
  putStrLn $ "You entered: " ++ result

-- List of IO actions for printing 'hello'
actions :: [IO ()]
actions = [putChar 'h', putChar 'e', putChar 'l', putChar 'l', putChar 'o']
