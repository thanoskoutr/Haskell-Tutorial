-- Import Modules
import Data.List

-- Import some functions from a Module
import Data.List (nub, sort)

-- Import all the functions from a Module except some
import Data.List hiding (nub)

-- Qualified Imports
import qualified Data.Map
-- If we want to use the filter function of that package we have
-- to call it like this: Data.Map.filter

-- Rename Qualified Imports
import qualified Data.Map as M
-- Now if we want to use the previous filter function we can
-- call it like this: M.filter


-- nub is a function that takes a list and weeds out duplicate elements
numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub



-- Data.List Functions
-- intersperse
intersperseExample = intersperse '.' "MONKEY"
-- result: "M.O.N.K.E.Y"

-- intercalate
intercalateExample = intercalate [0,0,0] [[1,2,3],[4,5,6],[7,8,9]]
-- result: [1,2,3,0,0,0,4,5,6,0,0,0,7,8,9]

-- transpose
transposeExample = transpose [[1,2,3],[4,5,6],[7,8,9]]
-- result: [[1,4,7],[2,5,8],[3,6,9]]

-- foldl', foldl1'
-- They are strict versions of the originals, which means that the 
-- accumulator gets computed as they go along, in order to reduce
-- stack overflow errors.

-- concat
concatExample = concat [[3,4,5],[2,3,4],[2,1,1]]
-- result: [3,4,5,2,3,4,2,1,1]

-- and, or
andExample = and [True, False, True]
-- result: False
orExample = or [True, False, True]
-- result: True

-- any, all
anyExample = any (==4) [2,3,5,6,1,4]
-- result: True
allExample = all (>4) [6,9,10]
-- result: True

-- iterate
iterateExample = take 10 $ iterate (*2) 1
-- result: [1,2,4,8,16,32,64,128,256,512]

-- splitAt
splitAtExample1 = splitAt 3 "heyman"
-- result: ("hey","man")
splitAtExample2 = splitAt 100 "heyman"
-- result: ("heyman","")
splitAtExample3 = splitAt (-3) "heyman"
-- result: ("","heyman")

-- takeWhile
takeWhileExample = takeWhile (>3) [6,5,4,3,2,1,2,3,4,5,4,3,2,1]
-- result: [6,5,4]
-- Problem: Find the sum of all third powers that are under 10,000
sumOf3rdPowersUnder10000 = sum $ takeWhile (<10000) $ map (^3) [1..]

-- dropWhile
dropWhileExample = dropWhile (<3) [1,2,2,2,3,4,5,4,3,2,1]
-- result: [3,4,5,4,3,2,1]

-- sort
sortExample = sort [8,5,3,2,1,6,4,2]
-- result: [1,2,2,3,4,5,6,8]

-- group
groupExample = group [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]
-- result: [[1,1,1,1],[2,2,2,2],[3,3],[2,2,2],[5],[6],[7]]

-- inits, tails
initsExample = inits "w00t"
-- result: ["","w","w0","w00","w00t"]
tailsExample = tails "w00t"
-- result: ["w00t","00t","0t","t",""]

-- isInfixOf, isPrefixOf, isSuffixOf

-- elem, notElem

-- partition
partitionExample = partition (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy"
-- result: ("BOBMORGAN","sidneyeddy")

-- find
-- find takes a list and a predicate and returns the first element that satisfies the predicate.
-- But it returns that element wrapped in a Maybe value.
-- A Maybe values is: Just something, or Nothing
-- This is made, so as not to have a runtime error if the element is never found
findExample1 = find (>4) [1,2,3,4,5,6]
-- result: Just 5
findExample2 = find (>9) [1,2,3,4,5,6]
-- result: Nothing

-- elemIndex
elemIndexExample1 = 4 `elemIndex` [1,2,3,4,5,6]
-- result: Just 3
elemIndexExample2 = 10 `elemIndex` [1,2,3,4,5,6]
-- result: Nothing

-- elemIndices
elemIndicesExample = ' ' `elemIndices` "Where are the spaces?"
-- result: [5,9,13] (The list of indices of where the elem is found)

-- findIndex
findIndexExample1 = findIndex (==4) [5,3,2,1,6,4]
-- result: Just 5 (The index of the first elem that satisfies the predicate)
findIndexExample2 = findIndex (==7) [5,3,2,1,6,4]
-- result: Nothing

-- zipWith3, zipWith4, ..., zipWith7
zipWith3Example = zipWith3 (\x y z -> x + y + z) [1,2,3] [4,5,2,2] [2,2,3]
-- result: [7,9,8]
zipWith4Example = zip4 [2,3,3] [2,2,2] [5,5,3] [2,2,2]
-- result: [(2,2,5,2),(3,2,5,2),(3,2,3,2)]


-- lines
-- It takes a string and returns every line of that string in a separate list.
linesExample = lines "first line\nsecond line\nthird line"
-- result: ["first line","second line","third line"]

-- unlines
unlinesExample = unlines ["first line", "second line", "third line"]
-- result: "first line\nsecond line\nthird line\n"

-- words, unwords
wordsExample = words "hey these are the words in this sentence"
-- result: ["hey","these","are","the","words","in","this","sentence"]
unwordsExample = unwords ["hey","there","mate"]
-- result: "hey there mate"

-- nub
nubExample = nub [1,2,3,4,3,2,1,2,3,4,3,2,1]
-- result: [1,2,3,4]

-- delete
deleteExample1 = delete 'h' "hey there ghang!"
-- result: "ey there ghang!"
deleteExample2 = delete 'h' . delete 'h' $ "hey there ghang!"
-- result: "ey tere ghang!"
deleteExample3 = delete 'h' . delete 'h' . delete 'h' $ "hey there ghang!"
-- result: "ey tere gang!"


-- \\
-- It is the list difference function.
-- It acts like a set difference, basically. 
-- For every element in the right-hand list, it removes a matching element in the left one.
listDiffernceExample1 = [1..10] \\ [2,5,9]
-- result: [1,3,4,6,7,8,10]
listDiffernceExample2 = "Im a big baby" \\ "big"
-- result: "Im a  baby"

-- union, intersect
unionExample1 = [1..7] `union` [5..10]
-- result: [1,2,3,4,5,6,7,8,9,10]
unionExample2 = "hey man" `union` "man what's up"
-- result: "hey manwt'sup"
intersectExample = [1..7] `intersect` [5..10]
-- result: [5,6,7]

-- insert
-- Takes an element and a list of elements that can be sorted and
-- inserts it into the last position where it's still less than or equal to the next element.
insertExample1 = insert 4 [3,5,1,2,8,2]
-- result: [3,4,5,1,2,8,2]
insertExample2 = insert 4 [1,3,4,4,1]
-- result: [1,3,4,4,4,1]


-- Generic Equivalents of common functions
-- They are made to work with Integral or Num typeclasses, and not only Int.
    -- genericLength
    -- genericTake
    -- genericDrop
    -- genericSplitAt
    -- genericIndex
    -- genericReplicate

-- The are made to take an equality function and then compare them by using that equality function.
    -- nubBy
    -- deleteBy
    -- unionBy
    -- intersectBy
    -- groupBy (group is the same as groupBy (==))

groupByExample =    let values = [-4.3, -2.4, -1.2, 0.4, 2.3, 5.9, 10.5, 29.1, 5.3, -2.4, -14.5, 2.9, 2.3]
                    in groupBy (\x y -> (x > 0) == (y > 0)) values
-- result: [[-4.3,-2.4,-1.2],[0.4,2.3,5.9,10.5,29.1,5.3],[-2.4,-14.5],[2.9,2.3]]

-- They are made to take a function and determine if one element is greater, smaller or equal to the other
    -- sortBy
    -- insertBy
    -- maximumBy
    -- minimumBy



-- Data.Char Functions

