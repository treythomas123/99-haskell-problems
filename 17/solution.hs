-- Split a list into two parts; the length of the first part is given.
--
-- Do not use any predefined predicates.
--
-- *Main> split "abcdefghik" 3
-- ("abc", "defghik")

myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake n [] = []
myTake n (x:xs) = [x] ++ myTake (n-1) xs

myDrop :: Int -> [a] -> [a]
myDrop 0 xs = xs
myDrop n [] = []
myDrop n (x:xs) = myDrop (n-1) xs

split :: [a] -> Int -> ([a],[a])
split xs n = (myTake n xs, myDrop n xs)
