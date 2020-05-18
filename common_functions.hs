map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' pred (x:xs)
    | pred x = x : filter' pred xs
    | otherwise = filter' pred xs

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = (f x y): zipWith' f xs ys

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' pred list@(x:xs)
    | pred x = x : takeWhile' pred xs
    | otherwise = []

foldl' f acc [] = acc
foldl' f acc (x:xs) = foldl' f (f acc x) xs

foldr' f acc [] = acc
foldr' f acc (x:xs) = f x (foldr' f acc xs)

sum' :: (Num a) => [a] -> a
sum' xs = foldl' (\acc x -> acc + x) 0 xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' x xs = foldl' (\acc y -> if y == x then True else acc) False xs

elem'' _ [] = False
elem'' x (y:ys)
    | x == y = True
    | otherwise = elem'' x ys

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr' (\x acc -> f x : acc) [] xs

-- map with foldr is preferred because ++ is more expensive than :
map''' :: (a -> b) -> [a] -> [b]
map''' f xs = foldl' (\acc x -> acc ++ [f x]) [] xs

-- foldl1 and foldr1 assume the starting value to be the first or last element

foldl1' :: (a -> a -> a) -> [a] -> a
foldl1' f [] = error "foldl1 called on empty list"
foldl1' f (x:xs) = foldl' f x xs

foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' f [] = error "foldr1 called on empty list"
foldr1' f [x] = x
foldr1' f (x:xs) = f x (foldr1' f xs)
