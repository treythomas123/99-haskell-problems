-- Run-length encoding of a list. Use the result of problem P09 to implement
-- the so-called run-length encoding data compression method. Consecutive
-- duplicates of elements are encoded as lists (N E) where N is the number of
-- duplicates of the element E.
--
-- Example:
--
-- encode "aaaabccaadeeee"
-- [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]

encode :: (Eq a) => [a] -> [(Int,a)]
encode [] = []
encode (x:xs) = [(length dupes, x)] ++ encode rest
    where split = span (==x) (x:xs)
          dupes = fst split
          rest = snd split
