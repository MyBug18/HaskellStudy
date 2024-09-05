import Control.Monad

-- One line to blow your mind
powerset :: [a] -> [[a]]
powerset = filterM (\_->[True,False])

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

-- To explain that function is first-class data type
list10 :: (Num a) => [(a -> a)]
list10 = [(+3), (*5), (negate)]

-- To explain about Currying
myAdd :: Int -> Int -> Int
myAdd = (+)

myAdd3 :: Int -> Int
myAdd3 = (+) 3

my3Add5 :: Int
my3Add5 = (+) 3 5

-- To explain about recursion
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = (f x):(myMap f xs)

myQuickSort :: (Ord a) => [a] -> [a]
myQuickSort [] = []
myQuickSort (x:xs) = (myQuickSort $ filter (<x) xs) ++ [x] ++ (myQuickSort $ filter (>=x) xs)

myAppend :: [a] -> [a] -> [a]
myAppend [] ys = ys
myAppend (x:xs) ys = x:(myAppend xs ys)

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + (myLength xs)

myFold :: (b -> a -> b) -> b -> [a] -> b
myFold _ acc [] = acc
myFold f acc (x:xs) = myFold f (f acc x) xs

-- With lambda expression
list11 :: [Int]
list11 = map (\x -> x + 3) [2,3,4]

myReverse :: [a] -> [a]
myReverse = foldl (\acc x -> x:acc) []

-- To explain about lazy evaluation
list20 :: [a]
list20 = map undefined []

list21 :: [Int]
list21 = [4,3,undefined,2,1]

list21One :: Int
list21One = list21 !! 4

list21Length :: Int
list21Length = length list21


take5 :: [a] -> [a]
take5 = take 5

list22 :: [Int]
list22 = [1..10]

list23 :: [Int]
list23 = [1..]

list24 :: [Int]
list24 = let temp = 0 : list23 in 1 : temp

fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- To explain about syntatic sugar
list31 :: [(Int, Int)]
list31 = [(x, y) | x <- [1,2,3], y <- [4,5,6]]

list32 :: [(Int, Int)]
list32 = [1,2,3] >>= (\x -> [4,5,6] >>= (\y -> return (x, y)))

list33 :: [(Int, Int)]
list33 = do
    x <- [1,2,3]
    y <- [4,5,6]
    return (x, y)

-- Putting (almost) everything together

list40 :: [Int]
list40 =
    let funcList :: [Int -> Int -> Int]
        funcList = [(-), (+), \x y -> x * x + y * y] in
    map (\f -> zipWith f (repeat 4) [1..]) funcList |> map (take 5) |> concat