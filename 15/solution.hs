-- Replicate the elements of a list a given number of times.
--
-- > repli "abc" 3
-- "aaabbbccc"

repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = take n (repeat x) ++ repli xs n
