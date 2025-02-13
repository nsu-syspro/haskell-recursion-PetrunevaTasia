{-# OPTIONS_GHC -Wall #-}
-- Note: the above pragma enables all warnings

module Task3 where

-----------------------
-- Helper type synonyms

type Peg = String
type Move = (Peg, Peg)

-----------------------

-- Usage examples
--
-- >>> hanoi 2 "a" "b" "c"
-- [("a","c"),("a","b"),("c","b")]

hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
    | n == 0 = []
    | n >= 1 = hanoi (n-1) a c b ++ [(a, b)] ++ hanoi (n-1) c b a
    | otherwise = error "Error from hanoi: There must be a positive number of disks."

