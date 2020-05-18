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
