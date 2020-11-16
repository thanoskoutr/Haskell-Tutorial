-- Import Modules
import Data.List
import Data.Function
import Data.Char
import qualified Data.Map as Map  
import qualified Data.Set as Set
import Geometry
import qualified Geometry.Sphere as Sphere  
import qualified Geometry.Cuboid as Cuboid  
import qualified Geometry.Cube as Cube

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

-- on Function (from Data.Function)
-- It is defined:
    -- on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
    -- f `on` g = \x y -> f (g x) (g y)
-- Example: 
    -- (==) `on` (> 0) returns an equality function that looks like
    -- \x y -> (x > 0) == (y > 0)




-- Data.Char Functions
-- Some predicates over characters:
    -- Functions that take a character and tell us whether some assumption about it is true or false.
    -- Type Signature: Char -> Bool
-- Some of them are:
    -- isControl: checks whether a character is a control character
    -- isSpace: checks whether a character is a white-space characters. That includes spaces, tab characters, newlines, etc.
    -- isLower: checks whether a character is lower-cased. 
    -- isUpper: checks whether a character is upper-cased.
    -- isAlpha: checks whether a character is a letter.
    -- isAlphaNum: checks whether a character is a letter or a number.
    -- isPrint: checks whether a character is printable. Control characters, for instance, are not printable.
    -- isDigit: checks whether a character is a digit.
    -- isOctDigit: checks whether a character is an octal digit.
    -- isHexDigit: checks whether a character is a hex digit.
    -- isLetter: checks whether a character is a letter.
    -- isMark: checks for Unicode mark characters. Those are characters that combine with preceding letters to form latters with accents. Use this if you are French.
    -- isNumber: checks whether a character is numeric.
    -- isPunctuation: checks whether a character is punctuation.
    -- isSymbol: checks whether a character is a fancy mathematical or currency symbol.
    -- isSeparator: checks for Unicode spaces and separators.
    -- isAscii: checks whether a character falls into the first 128 characters of the Unicode character set.
    -- isLatin1: checks whether a character falls into the first 256 characters of Unicode.
    -- isAsciiUpper: checks whether a character is ASCII and upper-case.
    -- isAsciiLower: checks whether a character is ASCII and lower-case.

-- Problem: Make a program that takes a username and the username can only be comprised of alphanumeric characters.
onlyAlphanumericUser1 = all isAlphaNum "bobby283"
-- result: True
onlyAlphanumericUser2 = all isAlphaNum "eddy the fish!"
-- result: False

-- Problem: Create the words function.
wordsAttempt1 = groupBy ((==) `on` isSpace) "hey guys its me"
-- result: ["hey"," ","guys"," ","its"," ","me"]
wordsAttempt2 = filter (not . any isSpace) . groupBy ((==) `on` isSpace) $ "hey guys its me"
-- result: ["hey","guys","its","me"]

-- GeneralCategory
-- Is a datatype that is also an enumeration, and itpresents us with 
-- a few possible categories that a character can fall into.
-- It is also part of the Eq typeclass, so we can test stuff like:
    -- generalCategory c == Space
-- It has a type of:
    -- generalCategory :: Char -> GeneralCategory
-- There are about 31 categories. Some of them are:
categorySpace = generalCategory ' '
categoryUppercaseLetter = generalCategory 'A'
categoryLowercaseLetter = generalCategory 'a'
categoryOtherPunctuation = generalCategory '.'
categoryDecimalNumber = generalCategory '9'

categoryList = map generalCategory " \t\nA9?|"
-- result: [Space,Control,Control,UppercaseLetter,DecimalNumber,OtherPunctuation,MathSymbol]

-- Char Functions
    -- toUpper: converts a character to upper-case. Spaces, numbers, and the like remain unchanged.
    -- toLower: converts a character to lower-case.
    -- toTitle: converts a character to title-case. For most characters, title-case is the same as upper-case.
    -- digitToInt: converts a character to an Int. To succeed, the character must be in the ranges '0'..'9', 'a'..'f' or 'A'..'F'.
    -- intToDigit: is the inverse function of digitToInt. It takes an Int in the range of 0..15 and converts it to a lower-case character.
    -- ord: converts characters to their corresponding numbers.
    -- chr: converts numbers to their corresponding characters.

digitToIntExample = map digitToInt "34538"
-- result: [3,4,5,3,8]
intToDigitExample = intToDigit 15
-- result: 'f'

ordExample = ord 'a'
-- result: 97
chrExample = chr 97
-- result: 'a'
mapOrdExample = map ord "abcdefgh"
-- result: [97,98,99,100,101,102,103,104]

-- Problem: Create a Ceasar cipher.
encode :: Int -> String -> String
encode shift msg =
    let ords = map ord msg
        shifted = map (+shift) ords
    in map chr shifted

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

encodeExample = encode 3 "Im a little teapot"
-- result: "Lp#d#olwwoh#whdsrw"
decodeExample = decode 3 "Lp#d#olwwoh#whdsrw"
-- result: "Im a little teapot"




-- Data.Map Functions
-- Example of a association list (dictionary), that stores key-value pairs
phoneBook = 
    [
        ("betty", "555-2938"),
        ("bonnie","452-2928"),
        ("patsy","493-2928"),
        ("lucille","205-2928"),
        ("wendy","939-8282"),
        ("penny","853-2492")
    ]

-- A function that looks up some value given a key
findKey :: (Eq k) => k -> [(k,v)] -> v
findKey key xs = snd . head . filter (\(k,v) -> key == k) $ xs

-- A function that looks up some value given a key, without Runtime Errors (uses Maybe values)
findKeyMaybe :: (Eq k) => k -> [(k,v)] -> Maybe v
findKeyMaybe key [] = Nothing
findKeyMaybe key ((k,v):xs) = 
    if key == k
        then Just v
    else
        findKeyMaybe key xs

-- The same function as before, but with foldr (curried)
findKeyFold :: (Eq k) => k -> [(k,v)] -> Maybe v
findKeyFold key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing

findKeyFoldExample1 = findKey "penny" phoneBook
-- result: Just "853-2492"
findKeyFoldExample2 = findKey "wilma" phoneBook
-- result: Nothing

-- Ofcourse, its better to work with the functions and Map datatype from the Data.Map module
-- because they are faster than association lists (implemented as trees internally).
-- We imports as: import qualified Data.Map as Map

-- Some basic Map functions:

-- fromList
-- Takes an association list (in the form of a list) and returns a map with the same associations.
fromListExample1 = Map.fromList [("betty","555-2938"),("bonnie","452-2928"),("lucille","205-2928")]
-- result: fromList [("betty","555-2938"),("bonnie","452-2928"),("lucille","205-2928")]
fromListExample2 = Map.fromList [(1,2),(3,4),(3,2),(5,5)]
-- result: fromList [(1,2),(3,2),(5,5)]

-- empty
emptyExample = Map.empty
-- result: fromList []
insertEmptyExample1 = Map.insert 3 100 Map.empty
-- result: fromList [(3,100)]
insertEmptyExample2 = Map.insert 5 600 (Map.insert 4 200 ( Map.insert 3 100  Map.empty))
-- result: fromList [(3,100),(4,200),(5,600)]
insertEmptyExample3 = Map.insert 5 600 . Map.insert 4 200 . Map.insert 3 100 $ Map.empty
-- result: fromList [(3,100),(4,200),(5,600)]

-- Problem: Implement fromList using empty map, insert and a fold.
fromList' :: (Ord k) => [(k,v)] -> Map.Map k v
fromList' = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty

-- null
nullExample1 = Map.null Map.empty
-- result: True
nullExample2 = Map.null $ Map.fromList [(2,3),(5,5)]
-- result: False

-- size
sizeExample1 = Map.size Map.empty
-- result: 0
sizeExample2 = Map.size $ Map.fromList [(2,4),(3,3),(4,2),(5,4),(6,4)]
-- result: 5

-- singleton (takes a key and a value and creates a map that has exactly one mapping)
singletonExample1 = Map.singleton 3 9
-- result: fromList [(3,9)]
singletonExample2 = Map.insert 5 9 $ Map.singleton 3 9
-- result: fromList [(3,9),(5,9)]

-- lookup (It returns Just something if it finds something for the key and Nothing if it doesn't)
lookupExample = Map.lookup 6 $ Map.fromList [(2,4),(3,3),(4,2),(5,4),(6,4)]
-- result: Just 4

-- member (Takes a key and a map and reports whether the key is in the map or not)
memberExample1 = Map.member 3 $ Map.fromList [(3,6),(4,3),(6,9)]
-- result: True
memberExample2 = Map.member 3 $ Map.fromList [(2,5),(4,5)]
-- result: False

-- map, filter (Same function as their list equivalents)
mapExample = Map.map (*100) $ Map.fromList [(1,1),(2,4),(3,9)]
-- result: fromList [(1,100),(2,400),(3,900)]
filterExample = Map.filter isUpper $ Map.fromList [(1,'a'),(2,'A'),(3,'b'),(4,'B')]
-- result: fromList [(2,'A'),(4,'B')]

-- toList (The inverse of fromList)
toListExample = Map.toList . Map.insert 9 2 $ Map.singleton 4 3
-- result: [(4,3),(9,2)]

-- key, elems (Return lists of keys and values respectively)
    -- keys is the equivalent of map fst . Map.toList
    -- elems is the equivalent of map snd . Map.toList.

-- fromListWith
    -- It acts like fromList, only it doesn't discard duplicate keys
    -- but it uses a function supplied to it to decide what to do with them
phoneBookDuplicates = 
    [
        ("betty", "555-2938"),
        ("betty","342-2492"),
        ("bonnie","452-2928"),
        ("patsy","493-2928"),
        ("patsy","943-2929"),
        ("patsy","827-9162"),
        ("lucille","205-2928"),
        ("wendy","939-8282"),
        ("penny","853-2492"),
        ("penny","555-2111")
    ]
-- Now if we just use fromList to put that into a map, we'll lose a few numbers. So here's what we'll do:
phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap xs = Map.fromListWith (\number1 number2 -> number1 ++ ", " ++ number2) xs

phoneBookToMapExample1 = Map.lookup "patsy" $ phoneBookToMap phoneBookDuplicates
-- result: "827-9162, 943-2929, 493-2928"
phoneBookToMapExample2 = Map.lookup "wendy" $ phoneBookToMap phoneBook
-- result: "939-8282"

-- Problem: Make a map from an association list of numbers and when a duplicate key is found, 
--          we want the biggest value for the key to be kept.
duplicateMapChooseMax = Map.fromListWith max [(2,3),(2,5),(2,100),(3,29),(3,22),(3,11),(4,22),(4,15)]
-- result: [(2,100),(3,29),(4,22)]

-- insertWith (It is to insert what fromListWith is to fromList)
    -- It inserts a key-value pair into a map, but if that map already contains the key, 
    -- it uses the function passed to it to determine what to do.
insertWithExample = Map.insertWith (+) 3 100 $ Map.fromList [(3,4),(5,103),(6,339)]
-- result: fromList [(3,104),(5,103),(6,339)]




-- Data.Set
    -- All the elements in a set are unique. 
    -- And because they're internally implemented with trees (much like maps in Data.Map), they're ordered.
    -- Checking for membership, inserting, deleting, etc. is much faster than doing the same thing with lists.
    -- The most common operation when dealing with sets are inserting into a set, checking for membership and converting a set to a list.
-- We imports as: import qualified Data.Set as Set

text1 = "I just had an anime dream. Anime... Reality... Are they so different?"
text2 = "The old man left his garbage can out and now his trash is all over my lawn!"

-- fromList (It takes a list and converts it into a set)
set1 = Set.fromList text1
-- result: fromList " .?AIRadefhijlmnorstuy"
set2 = Set.fromList text2
-- result: fromList " !Tabcdefghilmnorstuvwy"

-- intersection (Finds which elements are common between sets)
intersectionExample = Set.intersection set1 set2
-- result: fromList " adefhilmnorstuy"

-- difference (Finds which elements are in first set but aren't in the second one and vice versa)
differenceExample1 = Set.difference set1 set2
-- result: fromList ".?AIRj"
differenceExample2 = Set.difference set2 set1
-- result: fromList "!Tbcgvw"

-- union (Finds which elements are used in both sentences)
unionExample = Set.union set1 set2
-- result: fromList " !.?AIRTabcdefghijlmnorstuvwy"

-- null, size, member, empty, singleton, insert and delete (Work like you'd expect them to)
nullSetExample1 = Set.null Set.empty
-- result: True
nullSetExample2 = Set.null $ Set.fromList [3,4,5,5,4,3]
-- result: False
sizeSetExample = Set.size $ Set.fromList [3,4,5,3,4,5]
-- result: 3
singletonSetExample = Set.singleton 9
-- result: fromList [9]
insertSetExample1 = Set.insert 4 $ Set.fromList [9,3,8,1]
-- result: fromList [1,3,4,8,9]
insertSetExample2 = Set.insert 8 $ Set.fromList [5..10]
-- result: fromList [5,6,7,8,9,10]
deleteSetExample = Set.delete 4 $ Set.fromList [3,4,5,4,3,4,5]
-- result: fromList [3,5]

-- Check for subsets, proper subsets
isSubsetOfExample1 = Set.fromList [2,3,4] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]
-- result: True
isSubsetOfExample2 = Set.fromList [1,2,3,4,5] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]
-- result: True
isSubsetOfExample3 = Set.fromList [2,3,4,8] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]
-- result: False
isProperSubsetOfExample1 = Set.fromList [1,2,3,4,5] `Set.isProperSubsetOf` Set.fromList [1,2,3,4,5]
-- result: False

-- map, filter
mapSetExample = Set.filter odd $ Set.fromList [3,4,5,6,7,2,3,4]
-- result: fromList [3,5,7]
filterSetExample = Set.map (+1) $ Set.fromList [3,4,5,6,7,2,3,4]
-- result: fromList [3,4,5,6,7,8]

-- setNub (Works like nub, by weeding out duplicates from a list)
    -- But it does it by first making it into a set with fromList and then converting it back to a list with toList.
    -- Weeding out duplicates for large lists is much faster if you cram them into a set 
    --  and then convert them back to a list than using nub.
    -- But using nub only requires the type of the list's elements to be part of the Eq typeclass, 
    --  whereas if you want to cram elements into a set, the type of the list has to be in Ord.
setNub :: Ord a => [a] -> [a]
setNub xs = Set.toList $ Set.fromList xs

setNubExample = setNub "HEY WHATS CRACKALACKIN"
-- result: " ACEHIKLNRSTWY"
nubCounterExample = nub "HEY WHATS CRACKALACKIN"
-- result: "HEY WATSCRKLIN"




-- Making our own modules
    -- We will use the module we created in the file "Geometry.hs"
    -- We import it with:
        -- import Geometry
    -- "Geometry.hs" has to be in the same folder that the program that's importing it is in.

-- Modules can also be given a hierarchical structures.
    -- Each module can have a number of sub-modules and they can have sub-modules of their own.
    -- We will use the new module we created in the folder Geometry that has sub-modules.
    -- This way we can have functions like area, volume with the same name for Cuboid, Cube, Sphere
    --  because they are separete modules.
    -- If we want only a sub-module, we import it with (and use area, volume):
        -- import Geometry.Sphere
    -- If we want more than one sub-module, we import it with (and use Sphere.area, Sphere.volume, etc):
        -- import qualified Geometry.Sphere as Sphere  
        -- import qualified Geometry.Cuboid as Cuboid  
        -- import qualified Geometry.Cube as Cube
    -- Geometry folder has to be in the same folder that the program that's importing it is in.
