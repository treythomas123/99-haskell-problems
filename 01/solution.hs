-- Find the last element of a list.
--
-- Example in Haskell:
--
-- Prelude> myLast [1,2,3,4]
-- 4
-- Prelude> myLast ['x','y','z']
-- 'z'

myLast :: [a] -> a
myLast [] = error "empty list"
myLast (x:[]) = x
myLast (_:xs) = myLast xs
