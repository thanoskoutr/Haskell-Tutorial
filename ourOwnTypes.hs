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
