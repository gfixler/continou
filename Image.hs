module Image where

import Color

type Coord = (Double, Double)
type Image a = Coord -> a

always :: a -> Image a
always = const

leftAndRight :: a -> a -> Image a
leftAndRight l r = \(x,y) -> if x < 0 then l else r

lerp :: (Enum a, Fractional a) => a -> a -> Int -> [a]
lerp s e n = [(e - s) / n' * i + s | i <- [0..n']]
    where n' = fromIntegral (n - 1)

render :: Coord -> Coord -> Int -> Int -> Image a -> [[a]]
render (l,b) (r,t) w h f =
        [[f (x,y) | x <- lerp l r w] | y <- lerp t b h]

