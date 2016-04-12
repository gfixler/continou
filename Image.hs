module Image where

import Color

type Image = (Int, Int) -> Color

alwaysBlue :: Image
alwaysBlue = \(x,y) -> Blue

