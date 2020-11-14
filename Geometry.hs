-- module should be named after the filename
module Geometry
( sphereVolume
, sphereArea
, cubeVolume
, cubeArea
, cuboidArea
, cuboidVolume
) where

sphereVolume :: Float -> Float
sphereVolume radius = (4.0 / 3.0) * pi * (radius ^ 3)

sphereArea :: Float -> Float
sphereArea radius = 4 * pi * (radius ^ 2)

cubeVolume :: Float -> Float
cubeVolume side = cuboidVolume side side side

cubeArea :: Float -> Float
cubeArea side = cuboidArea side side side

cuboidVolume :: Float -> Float -> Float -> Float
cuboidVolume a b c = rectangleArea a b * c

cuboidArea :: Float -> Float -> Float -> Float
cuboidArea a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2

rectangleArea :: Float -> Float -> Float
rectangleArea a b = a * b

-- We also defined a helper function called rectangleArea, which calculates a rectangle's area based on the lenghts of its sides. 
-- Notice that we used it in our functions in the module (namely cuboidArea and cuboidVolume) but we didn't export it! 
-- Because we want our module to just present functions for dealing with three dimensional objects.

-- When making a module, we usually export only those functions that act as a sort of interface to our module so that the implementation is hidden. 
-- If someone is using our Geometry module, they don't have to concern themselves with functions that we don't export. 
-- We can decide to change those functions completely or delete them in a newer version (we could delete rectangleArea and just use * instead) 
--  and no one will mind because we weren't exporting them in the first place.