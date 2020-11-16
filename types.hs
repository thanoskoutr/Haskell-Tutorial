-- Expression Types
-- we can get the type with :t something
charType = 'a'
boolType = True
stringType = "hello"
tupleType = (True, "A")


-- Function Types
removeNonUppercase :: [Char] -> [Char]
-- or we could define its type with String:
-- removeNonUppercase :: String -> String
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z'] ]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z


-- Common Types:
    -- Int
    -- Integer
    -- Float
    -- Double
    -- Bool
    -- Char
    -- Ordering (GT, LT, EQ)


-- Type Variables
-- Like head, head :: [a] -> a

-- If we don't define its type, ghci will print:
-- addThreeGeneric :: Num a => a -> a -> a -> a
addThreeGeneric x y z = x + y + z


-- Typeclasses
  -- Class Constraint
    -- Everything before the symbol: =>, is a class constraint, e.g:
    -- :t addThreeGeneric
        -- addThreeGeneric :: Num a => a -> a -> a -> a
    -- :t (==)
        -- (==) :: (Eq a) => a -> a -> Bool


  -- Basic typeclasses:
    -- Eq:      is for types that support equality testing.
        -- Members: ==, /=

    -- Ord:     is for types that have an ordering.
        -- Members: >, <, >=, <=

    -- Show:    is for types that can be presented as strings.
string3 = show 3

    -- Read:    is for types that can be presented as a type.
readStringAsInt = read "5" - 2
readStringAsIntAnnotation = read "5" :: Int

    -- Enum:    is for types that can be enumerated
        -- Types in class: (), Bool, Char, Ordering, Int, Integer, Float, Double

    -- Bounded: is for types that have an upper and lower bound
minBoundOfInt = minBound :: Int
maxBoundOfInt = maxBound :: Int

    -- Num:     is for types that are numeric
        -- Types in class: Int, Integer, Float, Double

    -- Integral:    is a numeric class that includes only whole numbers
        -- Types in class: Int, Integer

    -- Floating:    is a numeric class that includes only floating point numbers
        -- Types in class: Float, Double

-- fromIntegral function, takes an integral number and turns it into a more general number
-- fromIntegral :: (Integral a, Num b) => a -> b
-- If we want to add the length of a list with 3.2, we will get an error because:
-- (Num b) => length :: [a] -> b
addFloatToLength = fromIntegral(length [1,2,3,4]) + 3.2
