-- Find the K'th element of a list. The first element in the list is number 1.
--
-- Example in Haskell:
--
-- Prelude> elementAt [1,2,3] 2
-- 2
-- Prelude> elementAt "haskell" 5
-- 'e'

elementAt :: [a] -> Int -> a
elementAt [] i = error "empty array"
elementAt (x:xs) i
    | i == 0         = x
    | i <= length xs = elementAt xs (i-1)
    | otherwise      = error "index out of bounds"
