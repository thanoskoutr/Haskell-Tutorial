module Geometry.Cube
( volume
, area
) where

-- We want to use functions from Geometry.Cuboid in Geometry.Cube but we can't just 
-- straight up do import Geometry.Cuboid because it exports functions with the same names 
-- as Geometry.Cube. That's why we do a qualified import and all is well.
import qualified Geometry.Cuboid as Cuboid

volume :: Float -> Float
volume side = Cuboid.volume side side side

area :: Float -> Float
area side = Cuboid.area side side side