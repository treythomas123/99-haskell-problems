-- Modified run-length encoding.
--
-- Modify the result of problem 10 in such a way that if an element has no
-- duplicates it is simply copied into the result list. Only elements with
-- duplicates are transferred as (N E) lists.
--
-- P11> encodeModified "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',
--  Multiple 2 'a',Single 'd',Multiple 4 'e']

import P09

data EncodedElement a = Multiple Int a | Single a deriving Show

encodeElement :: (Eq a) => [a] -> EncodedElement a
encodeElement [x] = Single x
encodeElement xs = Multiple (length xs) (head xs)

encodeModified :: (Eq a) => [a] -> [EncodedElement a]
encodeModified = map encodeElement . pack
