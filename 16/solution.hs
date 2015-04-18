-- Drop every N'th element from a list.
--
-- *Main> dropEvery "abcdefghik" 3
-- "abdeghk"

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n = take (n-1) xs ++ dropEvery (drop n xs) n
