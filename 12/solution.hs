-- Decode a run-length encoded list.
--
-- Given a run-length code list generated as specified in problem 11. Construct
-- its uncompressed version.
--
-- P12> decodeModified
--        [Multiple 4 'a',Single 'b',Multiple 2 'c',
--         Multiple 2 'a',Single 'd',Multiple 4 'e']
-- "aaaabccaadeeee"

data EncodedElement a = Multiple Int a | Single a deriving Show

decodeElement :: (Eq a) => EncodedElement a -> [a]
decodeElement (Single x) = [x]
decodeElement (Multiple n x) = take n (repeat x)

decodeModified :: (Eq a) => [EncodedElement a] -> [a]
decodeModified = foldl (++) [] . map decodeElement
