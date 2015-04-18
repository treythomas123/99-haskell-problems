-- Remove the K'th element from a list.
--
-- *Main> removeAt 2 "abcd"
-- ('b',"acd")

removeAt :: Int -> [a] -> (a, [a])
removeAt k xs = (removed, residue)
    where removed = xs !! (k-1)
          residue = take (k-1) xs ++ drop k xs
