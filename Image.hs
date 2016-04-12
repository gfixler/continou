module Image where

import Color

type Image a = (Double, Double) -> a

always :: a -> Image a
always = const

