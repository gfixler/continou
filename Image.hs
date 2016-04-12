module Image where

import Color

type Image a = (Double, Double) -> a

always :: a -> Image a
always = const

leftAndRight :: a -> a -> Image a
leftAndRight l r = \(x,y) -> if x < 0 then l else r

lerp :: (Enum a, Fractional a) => a -> a -> Int -> [a]
lerp s e n = [(e - s) / n' * i + s | i <- [0..n']]
    where n' = fromIntegral (n - 1)

render :: (Double,Double) -> (Double,Double) -> Int -> Int -> Image a -> [a]
render (l,b) (r,t) w h f = [f (x,y) | y <- lerp t b h, x <- lerp l r w]

