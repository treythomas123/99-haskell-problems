-- Run-length encoding of a list (direct solution).
--
-- Implement the so-called run-length encoding data compression method directly.
-- I.e. don't explicitly create the sublists containing the duplicates, as in
-- problem 9, but only count them. As in problem P11, simplify the result list
-- by replacing the singleton lists (1 X) by X.
--
-- P13> encodeDirect "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',
--  Multiple 2 'a',Single 'd',Multiple 4 'e']

data EncodedElement a = Multiple Int a | Single a deriving Show

encodeDirect :: (Eq a) => [a] -> [EncodedElement a]
encodeDirect [] = []
encodeDirect (x:xs) = encode count x : encodeDirect rest
    where count = 1 + (length $ takeWhile (== x) xs)
          rest = dropWhile (== x) xs
          encode 1 = Single
          encode n = Multiple n
