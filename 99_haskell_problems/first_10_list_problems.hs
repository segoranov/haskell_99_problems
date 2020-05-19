-- Problem 1
-- (*) Find the last element of a list.

-- Example in Haskell:
-- λ> myLast [1,2,3,4]
-- 4
-- λ> myLast ['x','y','z']
-- 'z'

myLast :: [a] -> a
myLast [] = error "Called last on empty list."
myLast [x] = x
myLast (_:xs) = myLast xs

myLast' xs = xs !! (length xs - 1)

myLast'' = head . reverse

-- Problem 2
-- (*) Find the last but one element of a list.

-- Example in Haskell:
-- λ> myButLast [1,2,3,4]
-- 3
-- λ> myButLast ['a'..'z']
-- 'y'

myButLast :: [a] -> a
myButLast = head . tail . reverse

myButLast' [] = error "myButLast called on empty list"
myButLast' [x] = error "myButLast called on one-element list"
myButLast' [x,y] = x
myButLast' (x:xs) = myButLast xs

-- Problem 3
-- (*) Find the K'th element of a list. The first element in the list is number 1.

-- Example in Haskell:
-- λ> elementAt [1,2,3] 2
-- 2
-- λ> elementAt "haskell" 5
-- 'e'

elementAt :: Int -> [a] -> a
elementAt _ [] = error "Called elementAt on empty list."
elementAt n list@(x:xs)
    | (n < 1 || n > length list) = error "Index out of bounds. N must be between 1 and length of list."
    | n == 1 = x
    | otherwise = elementAt (n-1) xs

elementAt' n xs = xs !! (n-1)

-- Problem 4
-- (*) Find the number of elements of a list.

-- Example in Haskell:
-- λ> myLength [123, 456, 789]
-- 3
-- λ> myLength "Hello, world!"
-- 13

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength' xs

myLength' = foldl (\acc x -> acc + 1) 0

-- TODO! Why does this does not compile? :  myLength'' = foldr (\x acc -> 1 + acc) 0
myLength'' xs = foldr (\x acc -> 1 + acc) 0 xs

-- Problem 5
-- (*) Reverse a list.

-- Example in Haskell:

-- λ> myReverse "A man, a plan, a canal, panama!"
-- "!amanap ,lanac a ,nalp a ,nam A"
-- λ> myReverse [1,2,3,4]
-- [4,3,2,1]

myReverse :: [a] -> [a]
myReverse = foldl (\acc x -> x : acc) []

myReverse' :: [a] -> [a]
myReverse' = foldl (flip (:)) []

myReverse'' [] = []
myReverse'' (x:xs) = myReverse'' xs ++ [x]

-- Problem 6
-- (*) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).

-- Example in Haskell:

-- λ> isPalindrome [1,2,3]
-- False
-- λ> isPalindrome "madamimadam"
-- True
-- λ> isPalindrome [1,2,4,8,16,8,4,2,1]
-- True

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = (reverse xs) == xs
