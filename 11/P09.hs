-- Pack consecutive duplicates of list elements into sublists. If a list
-- contains repeated elements they should be placed in separate sublists.
--
-- Example:
--
-- *Main> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a',
--              'a', 'd', 'e', 'e', 'e', 'e']
--              ["aaaa","b","cc","aa","d","eeee"]

module P09
( pack
) where

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:ys) = (x:dupes) : pack rest
    where dupes = takeWhile (== x) ys
          rest  = dropWhile (== x) ys
