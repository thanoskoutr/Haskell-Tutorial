-- Declare a list
lostNumbers = [4,8,15,16,23,42]

-- Add lists
lostNumbersAdded = lostNumbers ++ [1,2]

-- Concat to list
lostNumbersConcat = 3 : lostNumbers

-- Take number out of list
lostNumber = lostNumbers !! 2

-- Double lists
b = [[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3]]
b' = b ++ [[1,1,1,1]]
b2 = b !! 2     -- [1,2,2,3,4]
b22 = b2 !! 2   -- 2

--Compare lists
c = [3,2,1]
d = [2,1,0]
e = c > d   -- True

-- Functions on lists
lostHead = head lostNumbers -- 4
lostTail = tail lostNumbers -- [8,15,16,23,42]
lostLast = last lostNumbers -- 42
lostInit = init lostNumbers -- [4,8,15,16,23]

lostLength = length lostNumbers     -- 6
lostNull = null lostNumbers         -- False
lostReverse = reverse lostNumbers   -- [42,23,16,15,8,4]
lostTake = take 3 lostNumbers       -- [4,8,15]
lostDrop = drop 3 lostNumbers       -- [16,23,42]

lostElem  = 4 `elem` lostNumbers    -- True

-- More list functions: maximum, minimum, sum, product 
-- Usage: func [1,2,3,4]

-- Texas Ranges
oneTo20 = [1..20]
twoTo20step = [2,4..20]
aToz = ['a'..'z']

-- Take 24 multiples of 13
multOf13 = [13,26..24*13]
multOf13' = take 24 [13,26..]

-- Infinite list functions
lostCycle = take 6 (cycle [1,2,3])  -- [1,2,3,1,2,3]
lostRepeat = take 5 (repeat 3)      -- [3,3,3,3,3]
lostReplicate = replicate 5 3       -- [3,3,3]

-- List Comprehension
firstDoubles = [x*2 | x <- [1..10]]
firstDoublesCondition = [x*2 | x <- [1..10], x*2 >= 12]
-- Replaces each odd number > 10 with "BANG!" and each odd number < 10 with "BOOM!"
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
--  Compination of lists
listCompination = [ x*y | x <- [2,5,10], y <- [8,10,11] ]   -- 4x4 = 16 element list
-- Our version of length
length' xs = sum [ 1 | _ <- xs ]

-- Tuples
listOfTuples = [(1,2), (3,4), (1,5)]

-- Functions on tuples
fstOfTuple = fst (8,11)
sndOfTuple = snd (8,11)
-- Zip
zippedList = zip [1,2,3,4,5] [5,5,5,5,5]
zippedCouples = zip [1..5] ["one","two","three","four","five"]