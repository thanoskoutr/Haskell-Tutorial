-- Curried functions
multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

multTwoWithNine = multThree 9
result9x2x3 = multTwoWithNine 2 3           -- returns 9x2x3

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x
resultCompare = compareWithHundred 1        -- returns GT

-- Infix HOF
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)
resultDivideByTen = divideByTen 90          -- returns 9

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])
resultIsUpperAlphanum = isUpperAlphanum 'e' -- returns False

-- Functions as Parameters
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
resultApplyTwice = applyTwice (3:) [1]      -- returns [3,3,1]

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
resultZipWith = zipWith' (+) [1,2,3,4] [5,6,7,8]    --returns [6,8,10,12]

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x

flipBetter' :: (a -> b -> c) -> b -> a -> c
flipBetter' f y x = f x y

resultNoFlip = zip [1,2,3,4,5] "hello"      -- returns [(1,'h'),(2,'e'),(3,'l'),(4,'l'),(5,'o')]
resultFlip = flip' zip [1,2,3,4,5] "hello"  -- returns [('h',1),('e',2),('l',3),('l',4),('o',5)]


-- Map
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map f xs

resultMapList = map (+3) [1,5,3,1,6]        -- returns [4,8,6,4,9]
resultMapDoubleList = map (map (^2)) [[1,2],[3,4,5,6],[7,8]]    -- returns [[1,4],[9,16,25,36],[49,64]]

-- Filter
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs

resultFilter = filter (>3) [1,5,3,2,1,6,4]  -- returns [5,6,4]

-- Quicksort with Filter
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort (filter (<=x) xs)
        biggerSorted = quicksort (filter (>x) xs)
    in smallerSorted ++ [x] ++ biggerSorted

-- Problem: Find the largest number under 100,000 that's divisible by 3829
largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0

-- Problem: Find the sum of all odd squares that are smaller tha 10,000
oddSquareSum :: Integer
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

oddSquareSumListCompr :: (Integral a) => a
oddSquareSumListCompr = sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)])

-- takeWhile function:  It takes a predicate and a list and then goes from the
--                      beginning of the list and returns its elements while the
--                      predicate holds true.

-- Problem: Collatz Sequences (If numbers is even -> divide by 2, if odd -> mult by 3 and add 1)
-- For all starting numbers between 1 and 100, how many chains have a length greater that 15?

-- First, we write function that produces the chains (Collatz Sequences)
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n    = n : chain (n `div` 2)
    | otherwise = n : chain (n*3 + 1)

-- Then, we write the function that solves the problem
numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15


-- Lambdas (λ)
numLongChainsLambda :: Int
numLongChainsLambda = length (filter (\xs -> length xs > 15) (map chain [1..100]))

zipWithLambda :: [Double]
zipWithLambda = zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]

flipLambda :: (a -> b -> c) -> b -> a -> c
flipLambda f = \x y -> f y x


-- Folds (foldl, foldr)
sumFoldl :: (Num a) => [a] -> a
sumFoldl xs = foldl (\acc x -> acc + x) 0 xs

sumFoldlCurried :: (Num a) => [a] -> a
sumFoldlCurried = foldl (+) 0 

elemFoldl :: (Eq a) => a -> [a] -> Bool
elemFoldl y ys = foldl (\acc x -> if x == y then True else False) False ys

mapFoldl :: (a -> b) -> [a] -> [b]
mapFoldl f xs = foldl (\acc x -> acc ++ [f x]) [] xs

-- Much more efficient with Right Fold
mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr f xs = foldr (\x acc -> f x : acc) [] xs

-- foldl1, foldr1 work just like the originals, but they dont need a starting value.
-- They assume the first (or last) element of the list to be the starting value and
-- start the fold with the element next to it.
-- The only disadvantage, is that when called with empty lists the cause runtime errors.

maximumFoldr1 :: (Ord a) => [a] -> a
maximumFoldr1 = foldr1 (\x acc -> if x > acc then x else acc)

reverseFoldl :: [a] -> [a]
reverseFoldl = foldl (\acc x -> x : acc) []

productFoldr1 :: (Num a) => [a] -> a
productFoldr1 = foldr1 (*)

filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr p = foldr (\x acc -> if p x then x : acc else acc) []

headFoldr1 :: [a] -> a
headFoldr1 = foldr1 (\x _ -> x)

lastFoldl1 :: [a] -> a
lastFoldl1 = foldl1 (\_ x -> x)


-- Scans (scanl, scanr)
-- They are like folds, but they only report all the intermediate accumulatro states in the form of a list.
resultScanl = scanl (+) 0 [3,5,2,1] -- returns [0,3,8,10,11], same as [0, 0+3, 3+5, 8+2, 10+1]
resultScanr = scanr (+) 0 [3,5,2,1] -- returns [11,8,3,1,0],  same as [3+5+2+1, 11-3, 8-5, 3-2, 1-1]
-- When using scanl, the final result will be in the last element of the resulting list.
-- When using scanr, the final result will be in the head of the resulting list.

-- Problem: How many elements does it take for the sum of the roots of all
--          natural numbers to exceed 1000?
sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1


-- Function application with $
-- It is defined as:
    -- ($) :: (a -> b) -> a -> b
    -- f $ x = f x
-- It is a function application operator that is righ-associative, whereas a space is left-associative.
    -- left-associative: f a b c -> f (a (b (c)))
    -- righ-associative: f a b c -> ((f a) b) c)

-- So now we can ommit parenthesis like these examples:
withTheDollarSum = sum (map sqrt [1..130])
withOutDollarSum = sum $ map sqrt [1..130]

withTheDollarSqrt = sqrt (3 + 4 + 9)
withOutDollarSqrt = sqrt $ 3 + 4 + 9

withTheDollarSumFilter = sum (filter (> 10) (map (*2) [2..10]))
withOutDollarSumFilter = sum $ filter (> 10) $ map (*2) [2..10]


-- Function composition (.)
-- It is defined as:
    -- (.) :: (b -> c) -> (a -> b) -> a -> c
    -- f . g = \x -> f (g x)
-- Function f, must take as its parameter a value that has the same type as g return value.

-- Problem: Turn a list of numbers, to a list where all numbers are negative:
negativesLambda = map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]
negativesComposition = map (negate . abs) [5,-3,-6,7,-3,2,-19,24]

-- Function composition is right-associative, so we can compose many functions at a time.
    -- f (g (z x)) <==> (f . g . z) x
multipleLambdas = map (\xs -> negate (sum (tail xs))) [[1..5],[3..6],[1..7]]
multipleCompositions = map (negate . sum . tail) [[1..5],[3..6],[1..7]]

-- Function composition on function with multiple parameters.
multParamsNoComp = sum (replicate 5 (max 6.7 8.9))
multParamsComps = (sum . replicate 5 . max 6.7) 8.9
-- or
-- multParamsComps = sum . replicate 5 . max 6.7 $ 8.9


-- Point free style
-- Writing in point free style, is ommiting the same variables on both sides of the function definition.
sumNoPointFree' :: (Num a) => [a] -> a
sumNoPointFree' xs = foldl (+) 0 xs

sumPointFree' :: (Num a) => [a] -> a
sumPointFree' = foldl (+) 0

fnNoPointFree :: (RealFrac a, Integral b, Floating a) => a -> b
fnNoPointFree x = ceiling (negate (tan (cos (max 50 x))))

fnPointFree :: Double -> Integer
fnPointFree = ceiling . negate . tan . cos . max 50

oddSquareSumComp :: Integer
oddSquareSumComp = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

oddSquareSumCompReadable :: Integer
oddSquareSumCompReadable =
    let oddSquares = filter odd $ map (^2) $ [1..]
        belowLimit = takeWhile (<10000) oddSquares
    in sum belowLimit
