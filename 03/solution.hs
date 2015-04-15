-- Find the K'th element of a list. The first element in the list is number 1.
--
-- Example in Haskell:
--
-- Prelude> elementAt [1,2,3] 2
-- 2
-- Prelude> elementAt "haskell" 5
-- 'e'

elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt (x:xs) i =  elementAt xs (i-1)
elementAt _ _ = error "index out of bounds"
