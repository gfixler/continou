module Image where

import Color

type Image a = (Double, Double) -> a

alwaysBlue :: Image Color
alwaysBlue = \(x,y) -> Blue

