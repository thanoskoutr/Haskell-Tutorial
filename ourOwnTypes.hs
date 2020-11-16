import qualified Data.Map as Map

-- Algebraic data types

-- We make our own data type to represent a Shape (Circle or Rectangle)
data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
    -- Circle, Rectangle are value constructors
    -- They each have 3 and 4 fields (parameters) respectively
    -- The deriving at the end of a data declaration makes that type part of the Show typeclass
    --  so we can print our data type as a string.

-- Now we can make functions that use the new Shape type
surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- We can use the value constractors as functions.
-- Here, we map them and partially apply them to create a list of concentric circles with different radii:
concentricCircles = map (Circle 10 20) [4,5,6,6]
-- result: [Circle 10.0 20.0 4.0,Circle 10.0 20.0 5.0,Circle 10.0 20.0 6.0,Circle 10.0 20.0 6.0]

-- We create an intermediate data type that defines a point in two-dimensional space. 
-- Then we can use that to make our shapes more understandable.
data Point = Point Float Float deriving (Show)
data ShapePoint = CirclePoint Point Float | RectanglePoint Point Point deriving (Show)
-- Notice that when defining a point, we used the same name for the data type and the value constructor. 
-- This has no special meaning, although it's common to use the same name as the type 
--  if there's only one value constructor.
-- So now the Circle has two fields, one is of type Point and the other of type Float.

-- We have to adjust our surface function to reflect these changes
surfacePoint :: ShapePoint -> Float
surfacePoint (CirclePoint _ r) = pi * r ^ 2
surfacePoint (RectanglePoint (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

surfacePointRectangleExample = surfacePoint (RectanglePoint (Point 0 0) (Point 100 100))
-- result: 10000.0
surfacePointCircleExample = surfacePoint (CirclePoint (Point 0 0) 24)
-- result: 1809.5574

-- Nudge Function
--It takes a shape, the amount to move it on the x axis and the amount to move it on the y axis
--   and then returns a new shape that has the same dimensions, only it's located somewhere else.
nudge :: ShapePoint -> Float -> Float -> ShapePoint
nudge (CirclePoint (Point x y) r) a b = CirclePoint (Point (x+a) (y+b)) r
nudge (RectanglePoint (Point x1 y1) (Point x2 y2)) a b = RectanglePoint (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

nudgeExample = nudge (CirclePoint (Point 34 34) 10) 5 10
-- result: Circle (Point 39.0 44.0) 10.0

-- If we don't want to deal directly with points, we can make some auxilliary functions that 
--  create shapes of some size at the zero coordinates and then nudge those.
baseCircle :: Float -> ShapePoint
baseCircle r = CirclePoint (Point 0 0) r

baseRect :: Float -> Float -> ShapePoint
baseRect width height = RectanglePoint (Point 0 0) (Point width height)

nudgeBaseRectExample = nudge (baseRect 40 100) 60 23
-- result: Rectangle (Point 60.0 23.0) (Point 100.0 123.0)

-- Exporting our Function and Types in a module
-- We could export them like this, in the begining of the file:
-- module Shapes
-- ( Point(..)
-- , Shape(..)
-- , surface
-- , nudge
-- , baseCircle
-- , baseRect
-- ) where

-- By doing Shape(..), we exported all the value constructors for Shape, 
--  so that means that whoever imports our module can make shapes by using the Rectangle and Circle value constructors.
-- We could also opt not to export any value constructors for Shape by just writing Shape in the export statement. 
-- That way, someone importing our module could only make shapes by using the auxilliary functions 
--  baseCircle and baseRect.




-- Record syntax

-- We create a data type that describes a person. 
-- The info that we want to store about that person is: 
    -- first name
    -- last name
    -- age
    -- height
    -- phone number
    -- favorite ice-cream flavor

-- Instead of defining our type and fields like this (as previous):
data PersonBAD = PersonBAD String String Int Float String String deriving (Show)
guy = PersonBAD "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"
-- result: Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"

-- An alternative way to define these fields, is with record syntax:
data Person = Person    { firstName :: String
                        , lastName :: String
                        , age :: Int
                        , height :: Float
                        , phoneNumber :: String
                        , flavor :: String
                        } deriving (Show)

-- The main benefit of this is that it creates functions that lookup fields in the data type. 
-- By using record syntax to create this data type, Haskell automatically made these functions: 
    -- firstName, lastName, age, height, phoneNumber and flavor.
-- There's another benefit to using record syntax. When we derive Show for the type, 
--  it displays it differently if we use record syntax to define and instantiate the type.

personExample = Person {
                        firstName = "Buddy", 
                        lastName = "Finklestein", 
                        age = 43, 
                        height = 184.2, 
                        phoneNumber = "526-2928", 
                        flavor = "Chocolate" 
                       }
-- When making a new person, we don't have to necessarily put the fields in the proper order, 
--  as long as we list all of them. But if we don't use record syntax, we have to specify them in order.




-- Type Parameters

-- A value constructor can take some values parameters and then produce a new value.
-- In a similar manner, type constructors can take types as parameters to produce new types.
-- Some example of these types are:
    -- Maybe (data Maybe a = Nothing | Just a), the a here is the type parameter.
        -- Depending on what we want this data type to hold when it's not Nothing, 
        -- this type constructor can end up producing a type of Maybe Int, Maybe Car, Maybe String.
        -- No value can have a type of just Maybe, because that's not a type per se, it's a type constructor.
    -- Lists, list type takes a parameter to produce a concrete type.
        -- Values can have an [Int] type, a [Char] type, a [[String]] type, 
        -- But you can't have a value that just has a type of [].

maybeExampleString = Just "Haha"
-- :t maybeExampleString
-- result: JmaybeExampleString :: Maybe [Char]
maybeExampleNum = Just 84
-- t: maybeExampleNum
-- result: maybeExampleNum :: (Num t) => Maybe t
maybeExampleNothing = Nothing
-- t: maybeExampleNothing
-- result: maybeExampleNothing :: Maybe a

-- If we were defining a mapping type, we could add a typeclass constraint in the data declaration:
    -- data (Ord k) => Map k v = ...
-- However, it's a very strong convention in Haskell to never add typeclass constraints in data declarations,
-- even if it seems to make sense, because you'll have to put them into the function type declarations either way,
-- when you are making a function with these data type.

-- We will implement a 3D vector type and add some operations for it. 
-- We'll be using a parameterized type because even though it will usually contain numeric types, 
--  it will still support several of them.
data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n

-- These functions can operate on types of Vector Int, Vector Integer, Vector Float, 
--  whatever, as long as the a from Vector a is from the Num typeclass.

-- Also, from the type declaration for these functions, you'll see that they can operate only on vectors
--  of the same type and the numbers involved must also be of the type that is contained in the vectors.

-- Notice that we didn't put a Num class constraint in the data declaration, 
--  because we'd have to repeat it in the functions anyway.

-- Once again, it's very important to distinguish between the type constructor and the value constructor.
-- When declaring a data type, 
    -- The part before the = is the type constructor.
    -- The constructors after it (possibly separated by |'s) are value constructors.

vectorAdd1Example = Vector 3 5 8 `vplus` Vector 9 2 8
-- result: Vector 12 7 16
vectorAdd2Example = Vector 3 5 8 `vplus` Vector 9 2 8 `vplus` Vector 0 2 3
-- result: Vector 12 9 19
vectorMultExample = Vector 3 9 7 `vectMult` 10
-- result: Vector 30 90 70
vectorScalarExample = Vector 4 9 5 `scalarMult` Vector 9.0 2.0 4.0  
-- result: 74.0
vectorMultAndScalarExample = Vector 2 9 3 `vectMult` (Vector 4 9 5 `scalarMult` Vector 9 2 4)
-- result: Vector 148 666 222




-- Derived instances

-- A typeclass is a sort of an interface that defines some behavior. 
-- A type can be made an instance of a typeclass if it supports that behavior.
    -- Example: the Int type is an instance of the Eq typeclass because the Eq typeclass 
    --  defines behavior for stuff that can be equated. And because integers can be equated, 
    --  Int is a part of the Eq typeclass.
-- The real usefulness comes with the functions that act as the interface for Eq, namely == and /=. 
-- If a type is a part of the Eq typeclass, we can use the == functions with values of that type.

-- Haskell can automatically make our type an instance of any of the following typeclasses: 
    -- Eq, Ord, Enum, Bounded, Show, Read
-- Haskell can derive the behavior of our types in these contexts if we use the **deriving** 
--  keyword when making our data type.

-- Consider the example:
data PersonDerived = PersonDerived  { firstNameD :: String
                                    , lastNameD :: String
                                    , ageD :: Int
                                    } deriving (Eq, Show, Read)
-- When we derive the Eq instance for a type and then try to compare two values of that type 
-- with == or /=, Haskell will see if the value constructors match and then it will check 
-- if all the data contained inside matches by testing each pair of fields with ==. 
-- The types of all the fields also have to be part of the Eq typeclass. 
-- But since both String and Int are, we're OK. 


-- Let's test our Eq instance.
mikeD = PersonDerived {firstNameD = "Michael", lastNameD = "Diamond", ageD = 43}
adRock = PersonDerived {firstNameD = "Adam", lastNameD = "Horovitz", ageD = 41}
mca = PersonDerived {firstNameD = "Adam", lastNameD = "Yauch", ageD = 44}

checkEQ1 = mca == adRock
-- result: False
checkEQ2 = mikeD == adRock
-- result: False
checkEQ3 = mikeD == mikeD
-- result: True
checkEQ4 = mikeD == PersonDerived {firstNameD = "Michael", lastNameD = "Diamond", ageD = 43}
-- result: True

-- Since Person is now in Eq, we can use it as the a for all functions that have a 
--  class constraint of Eq a in their type signature, such as elem.
beastieBoys = [mca, adRock, mikeD]
checkElemBB = mikeD `elem` beastieBoys
-- result: True


-- Let's test our Show instance.
mikeDagain = mikeD
-- result: PersonDerived {firstNameD = "Michael", lastNameD = "Diamond", ageD = 43}
mikeDagainConcat = "mikeD is: " ++ show mikeD
-- result: "mikeD is: PersonDerived {firstNameD = \"Michael\", lastNameD = \"Diamond\", ageD = 43}"


-- Let's test our Read instance.
readmikedD = read "PersonDerived {firstNameD =\"Michael\", lastNameD =\"Diamond\", ageD = 43}" :: PersonDerived
-- result: PersonDerived {firstNameD = "Michael", lastNameD = "Diamond", ageD = 43}
readmikedDandTest = read "PersonDerived {firstNameD =\"Michael\", lastNameD =\"Diamond\", ageD = 43}" == mikeD
-- result: True


-- We can also derive instances for the Ord type class, 
--  which is for types that have values that can be ordered.
-- If we compare two values of the same type that were made using different constructors,
--   the value which was made with a constructor that's defined first is considered smaller.

-- For example, Bool type, which can have a value of either False or True, we can think of it as being implemented like this:
    -- data Bool = False | True deriving (Ord)
-- Because the False value constructor is specified first and the True value constructor 
--  is specified after it, we can consider True as greater than False.
compareTrueFalse = True `compare` False
-- result: GT
compareTrueFalse1 = True > False
-- result: True
compareTrueFalse2 = True < False
-- result: False

-- For example, Maybe a data type, the Nothing value constructor is specified before 
--  the Just value constructor, so:
    -- A value of Nothing is always smaller than a value of Just something, 
    --  even if that something is minus one billion trillion. 
    -- But if we compare two Just values, then it goes to compare what's inside them.
compareNothingJust1 = Nothing < Just 100
-- result: True
compareNothingJust2 = Nothing > Just (-49999)
-- result: False
compareJustJust1 = Just 100 > Just 50
-- result: True
compareJustJust = Just 3 `compare` Just 2
-- result: GT


-- We can use algebraic data types to make enumerations with the Enum and Bounded typeclasses.
-- Consider the following data type:
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
            deriving (Eq, Ord, Show, Read, Bounded, Enum)
-- Because all the value constructors are nullary (take no parameters, i.e. fields), 
--  we can make it part of the Enum typeclass. 
-- The Enum typeclass is for things that have predecessors and successors. 
-- We can also make it part of the Bounded typeclass, which is for things 
--  that have a lowest possible value and highest possible value.

-- We can we can convert values of this type to and from strings (Show and Read typeclasses):
showDay = show Wednesday
-- result: "Wednesday"
readDay = read "Saturday" :: Day
-- result: Saturday

-- We can compare or equate days (Eq and Ord typeclasses).
compareDays1 = Saturday == Sunday
-- result: False
compareDays2 = Saturday == Saturday
-- result: True
compareDays3 = Saturday > Friday
-- result: True
compareDays4 = Monday `compare` Wednesday
-- result: LT

-- We can get the lowest and highest day (Bounded typeclass).
minimumDay = minBound :: Day
-- result: Monday
maximumDay = maxBound :: Day
-- result: Sunday

--We can get predecessors/successors of days and we can make list ranges from them (Enum typeclass).
nextDay = succ Monday
-- result: Tuesday
previousDay = pred Saturday
-- result: Friday
makeListRangeDay = [Thursday .. Sunday]
-- result: [Thursday,Friday,Saturday,Sunday]
makeListRangeDayBounds = [minBound .. maxBound] :: [Day]
-- result: [Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday]




-- Type synonyms

-- Type synonyms don't really do anything per se, they're just about giving some types 
--  different names so that they make more sense to someone reading our code and documentation.
    -- type String = [Char]
-- We've introduced the type keyword. 
-- The keyword might be misleading to some, because we're not actually making anything new 
--  (we did that with the data keyword), but we're just making a synonym for an already existing type.

phoneBook :: [(String,String)]
phoneBook =
    [("betty","555-2938")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ]

-- We see that the type of phoneBook is [(String,String)]. 
-- That tells us that it's an association list that maps from strings to strings, but not much else. 
-- Let's make a type synonym to convey some more information in the type declaration:
type PhoneBookType = [(String,String)]

-- We can make it more descriptive, by making a type synonym for String as well:
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name,PhoneNumber)]

-- So now, when we implement a function that takes a name and a number and sees 
--  if that name and number combination is in our phonebook, 
--  we can give it a very pretty and descriptive type declaration.
inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook


-- Type synonyms can also be parameterized. 
-- If we want a type that represents an association list type but still want it to be general 
--  so it can use any type as the keys and values, we can do this:
type AssocList k v = [(k,v)]
-- Now, a function that gets the value by a key in an association list can have a type of:
    -- (Eq k) => k -> AssocList k v -> Maybe v
-- AssocList is a type constructor that takes two types and produces a concrete type, like:
    -- AssocList Int String


-- Just like we can partially apply functions to get new functions,
--   we can partially apply type parameters and get new type constructors from them.
-- If we wanted a type that represents a map (from Data.Map) from integers to something, 
--  we could either do this:
type IntMap1 v = Map.Map Int v
-- Or we could do it like this:
type IntMap2 = Map.Map Int


-- Another cool data type that takes two types as its parameters is the Either a b type. 
-- This is roughly how it's defined:
    -- data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)
-- It has two value constructors. 
    -- If the Left is used, then its contents are of type a and 
    -- If Right is used, then its contents are of type b.
rightExample = Right 20
-- result: Right 20
leftExample = Left "w00t"
-- result: Left "w00t"


-- When we're interested in how some function failed or why, we usually use the result type of 
--  Either a b, instead of Maybe a, because Nothing doesn't really convey much information other than that something has failed.
    --  a is some sort of type that can tell us something about the possible failure and 
    -- b is the type of a successful computation. 
-- Hence, errors use the Left value constructor while results use Right.

-- Problem as an Example:
-- A high-school has lockers so that students have some place to put their Guns'n'Roses posters.
-- Each locker has a code combination. When a student wants a new locker, they tell the locker 
-- supervisor which locker number they want and he gives them the code. 
-- However, if someone is already using that locker, he can't tell them the code for the locker 
-- and they have to pick a different one.
data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)
-- And now, we're going to make a function that searches for the code in a locker map.
-- We're going to use an Either String Code type to represent our result, 
-- because our lookup can fail in two ways 
    -- The locker can be taken, in which case we can't tell the code or 
    -- The locker number might not exist at all. 
    -- If the lookup fails, we're just going to use a String to tell what's happened.

lockerLookup :: Int -> LockerMap -> Either String Code  
lockerLookup lockerNumber map =
    case Map.lookup lockerNumber map of
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
        Just (state, code) -> if state /= Taken
                                then Right code
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken,"ZD39I"))
    ,(101,(Free,"JAH3I"))
    ,(103,(Free,"IQSA9"))
    ,(105,(Free,"QOTSA"))
    ,(109,(Taken,"893JJ"))
    ,(110,(Taken,"99292"))
    ]

-- Now let's try looking up some locker codes:
lockerLookupExample1 = lockerLookup 101 lockers
-- result: Right "JAH3I"
lockerLookupExample2 = lockerLookup 100 lockers
-- result: Left "Locker 100 is already taken!"
lockerLookupExample3 = lockerLookup 102 lockers
-- result: Left "Locker number 102 doesn't exist!"
lockerLookupExample4 = lockerLookup 110 lockers
-- result: Left "Locker 110 is already taken!"
lockerLookupExample5 = lockerLookup 105 lockers
-- result: Right "QOTSA"




-- Recursive data structures

-- As we've seen, a constructor in an algebraic data type can have several 
--  (or none at all) fields and each field must be of some concrete type.
-- We can make types whose constructors have fields that are of the same type.

-- Using that, we can create recursive data types, where one value of some type
--  contains values of that type, which in turn contain more values of the same type and so on.

-- Let's use algebraic data types to implement our own list then:
data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
-- It's either an empty list or a combination of a head with some value and a list.
listEmptyExample = Empty
-- result: Empty
list1Example = 5 `Cons` Empty
-- result: Cons 5 Empty
list2Example = 4 `Cons` (5 `Cons` Empty)
-- result: Cons 4 (Cons 5 Empty)
list3Example = 3 `Cons` (4 `Cons` (5 `Cons` Empty))
-- result: Cons 3 (Cons 4 (Cons 5 Empty))

-- We can define functions to be automatically infix by making them comprised of only special characters. 
-- We can also do the same with constructors, since they're just functions that return a data type. 
-- So check this out:
infixr 5 :-:
data ListInfix a = EmptyInfix | a :-: (ListInfix a) deriving (Show, Read, Eq, Ord)
-- When we define functions as operators, we can use fixity declarations to give them a fixity (but we don't have to).
-- A fixity states how tightly the operator binds and whether it's left-associative or right-associative.
-- For instance:
    -- *'s fixity is infixl 7 * and 
    -- +'s fixity is infixl 6
-- That means that they're both left-associative, but * binds tighter than +, because it has a greater fixity.

-- Now, we can write out lists in our list type like so:
listInfixExample = 3 :-: 4 :-: 5 :-: EmptyInfix
-- result: 3 :-: (4 :-: (5 :-: EmptyInfix))
aList = 3 :-: 4 :-: 5 :-: EmptyInfix
bList = 100 :-: aList
-- result: 100 :-: (3 :-: (4 :-: (5 :-: EmptyInfix)))

-- When deriving Show for our type, Haskell will still display it as if the constructor was 
-- a prefix function, hence the parentheses around the operator (remember, 4 + 3 is (+) 4 3).

-- Let's make a function that adds two of our lists together. 
-- This is how ++ will be defined for our lists:
infixr 5 .++
(.++) :: ListInfix a -> ListInfix a -> ListInfix a
EmptyInfix .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

aListAdd = 3 :-: 4 :-: 5 :-: EmptyInfix
-- result: 3 :-: (4 :-: (5 :-: EmptyInfix))
bListAdd = 6 :-: 7 :-: EmptyInfix
-- result: 6 :-: (7 :-: EmptyInfix)
abListAdd = aListAdd .++ bListAdd
-- result: 3 :-: (4 :-: (5 :-: (6 :-: (7 :-: EmptyInfix))))


-- Binary Search Trees
-- We'll be implementing normal binary search trees.
-- A tree is either an empty tree or it's an element that contains some value and two trees. 
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

-- We will make a utility function for making a singleton tree (a tree with just one node) 
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

-- We will make a function to insert an element into a tree.
-- We do this by comparing the value we want to insert to the root node and then:
    -- if it's smaller, we go left, 
    -- if it's larger, we go right. 
-- We do the same for every subsequent node until we reach an empty tree. 
-- Once we've reached an empty tree, we just insert a node with that value instead of the empty tree.
treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)

-- We will make a function that checks if some element is in the tree.
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a  = treeElem x left
    | x > a  = treeElem x right

-- We will use a fold to build up a tree from a list (Instead of manually building one).
nums = [8,6,4,1,7,3,5]
numsTree = foldr treeInsert EmptyTree nums
-- result: Node 5 (Node 3 (Node 1 EmptyTree EmptyTree) (Node 4 EmptyTree EmptyTree)) (Node 7 (Node 6 EmptyTree EmptyTree) (Node 8 EmptyTree EmptyTree))

-- We will check if some elements are in our tree:
checkIfInTree1 = 8 `treeElem` numsTree
-- result: True
checkIfInTree2 = 100 `treeElem` numsTree
-- result: False
checkIfInTree3 = 1 `treeElem` numsTree
-- result: True
checkIfInTree4 = 10 `treeElem` numsTree
-- result: False




-- Typeclasses 102