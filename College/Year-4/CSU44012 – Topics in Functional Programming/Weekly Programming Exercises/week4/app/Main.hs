module Main where

import Control.Monad (ap, liftM)  -- Needed for Applicative and Functor
import Data.Monoid (mappend, mempty) -- Needed for handling logs

-- The Writer Monad holds a result and a log.
newtype Writer w a = Writer { runWriter :: (a, w) }

-- Monad implementation for Writer
instance Monoid w => Monad (Writer w) where
    -- return creates a Writer with an empty log.
    return x = Writer (x, mempty)
    -- >>= combines the logs of two Writer actions.
    (Writer (x, log)) >>= f = 
        let (Writer (y, newLog)) = f x
        in Writer (y, log `mappend` newLog)

-- Applicative implementation for Writer
instance Monoid w => Applicative (Writer w) where
    pure = return
    (<*>) = ap

-- Functor implementation for Writer
instance Monoid w => Functor (Writer w) where
    fmap = liftM

-- tell adds a new log entry.
tell :: Monoid w => w -> Writer w ()
tell log = Writer ((), log)

-- test of using the Writer Monad to log actions.
test :: Writer [String] Int
test = do
    tell ["entry a"]
    tell ["entry b"]
    return (2 + 1)

-- Main function to print the result and log of the test.
main :: IO ()
main = print $ runWriter test  -- Outputs: ((3, ["entry a", "entry b"]))

-- Part 2: Generalizing the Writer Monad
-- Problem when trying to use any type as a log: Not all types can be combined.
-- Monoid provides a way to combine logs, but not all types are Monoids.
-- Possible Solution: Create a new type class to specify how to combine logs.
