-- Rotate a list N places to the left.
--
-- Hint: Use the predefined functions length and (++).
--
-- Examples in Haskell:
--
-- *Main> rotate ['a','b','c','d','e','f','g','h'] 3
-- "defghabc"
--
-- *Main> rotate ['a','b','c','d','e','f','g','h'] (-2)
-- "ghabcdef"

rotate :: [a] -> Int -> [a]
rotate xs 0 = xs
rotate [x] _ = [x]
rotate (x:xs) n = rotate (xs ++ [x]) (r-1)
    where r = mod n (1 + length xs)
