module Image where

import Color

type Image a = (Double, Double) -> a

alwaysTrue :: Image Bool
alwaysTrue = \(x,y) -> True

alwaysBlue :: Image Color
alwaysBlue = \(x,y) -> Blue

