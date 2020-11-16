-- Declare Functions
doubleMe x = x + x

-- doubleUs x y = x*2 + y*2
-- doubleUs x y = x+x + y+y
doubleUs x y = doubleMe x + doubleMe y

-- Conditionals
doubleSmallNumber x = if x > 100
                        then x
                        else x*2

doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

-- Pattern Matching
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

head' [] = error "Can't call head on empty list"
head' (x:_) = x

length' [] = 0
length' (_:xs) = 1 + length' xs

sum' [] = 0
sum' (x:xs) = x + length' xs

-- Patterns
capital "" = "Empty  string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- Guards
bmiTell bmi
    | bmi <= 18.5   = "Eat something"
    | bmi <= 25.0   = "Normie"
    | bmi <= 30.0   = "You are fat"
    | otherwise     = "You are a whale"

bmiCalc weight height
    | weight / height ^ 2 <= 18.5   = "Eat something"
    | weight / height ^ 2 <= 25.0   = "Normie"
    | weight / height ^ 2 <= 30.0   = "You are fat"
    | otherwise                     = "You are a whale"

max' a b
    | a > b     = a
    | otherwise = b

-- Define infix
a `myCompare` b
    | a > b     = GT
    | a == b    = EQ
    | otherwise = LT

-- Where keyword
bmiCalcWhere :: (RealFloat a) => a -> a -> String
bmiCalcWhere weight height
    | bmi <= skinny = "Eat something"
    | bmi <= normal = "Normie"
    | bmi <= fat    = "You are fat"
    | otherwise     = "You are a whale"
    where   bmi = weight / height ^ 2
            skinny = 18.5
            normal = 25.0
            fat = 30.0

bmiCalcLists :: (RealFloat a) => [(a,a)] -> [a]
bmiCalcLists xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where   (f:_) = firstname
            (l:_) = lastname

-- Let keyword
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in sideArea + 2 * topArea

inlineLet = 4 * (let a = 9 in a + 1) + 2

inlineLets = (let a = 100; b = 200; c = 300 in a*b*c, let foo = "Hey"; bar = "There" in foo ++ bar)

patternMatchLet = (let (a,b,c) = (1,2,3) in a+b+c) * 100

bmiCalcListsLet :: (RealFloat a) => [(a,a)] -> [a]
bmiCalcListsLet xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

-- Case expressions
headCase ::  [a] -> a
headCase xs = case xs of    [] -> error "Can't call head on empty list"
                            (x:_) -> x

describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of  [] -> "empty."  
                                                [x] -> "a singleton list."   
                                                xs -> "a longer list."  