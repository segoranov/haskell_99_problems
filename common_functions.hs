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

maximum' :: (Ord a) => [a] -> a
maximum' = foldr1' (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl' (\acc x -> x:acc) []

product' :: (Num a) => [a] -> a
product' = foldr1' (*)

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' pred = foldr' (\x acc -> if pred x then x : acc else acc) []

head' :: [a] -> a
head' = foldr1' (\x _ -> x)

last' :: [a] -> a
last' = foldl1' (\_ x -> x)

-- scanl implemented with recursion
scanl' :: (b -> a -> b) -> b -> [a] -> [b]
scanl' f acc [] = [acc]
scanl' f acc (x:xs) = [acc] ++ scanl' f (f acc x) xs

-- scanl implemented with foldl
scanl'' f acc = foldl' (\acc x -> acc ++ [f (last' acc) x]) [acc]

-- scanr implemented with recursion
scanr' f acc [] = [acc]
scanr' f acc (x:xs) = f x acc0 : accs
    where accs@(acc0:_) = scanr f acc xs

-- scanr implemented with foldr
scanr'' f acc = foldr' (\x acc -> f x (head' acc) : acc) [acc]

group' :: (Eq a) => [a] -> [[a]]
group' [] = []
group' list@(x:xs) = listEqToFirst : group' (drop (length listEqToFirst) list) where
    listEqToFirst = takeWhile (==x) list

drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' n list@(x:xs)
    | n == 0 = list
    | otherwise = drop' (n-1) xs
