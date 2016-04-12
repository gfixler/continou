module Image where

import Color

type Image = (Double, Double) -> Color

alwaysBlue :: Image
alwaysBlue = \(x,y) -> Blue

