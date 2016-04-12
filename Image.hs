{-# LANGUAGE DeriveFunctor #-}

module Image where

import Color

type Coord = (Double, Double)
type Image a = Coord -> a
newtype Grid a = Grid { runGrid :: [[a]] } deriving Functor

instance Show a => Show (Grid a) where
    show = unlines . map (concatMap show) . runGrid

lerp :: (Enum a, Fractional a) => a -> a -> Int -> [a]
lerp s e n = [(e - s) / n' * i + s | i <- [0..n']]
    where n' = fromIntegral (n - 1)

grid :: Coord -> Coord -> Int -> Int -> Grid Coord
grid (l,b) (r,t) w h =
        Grid [[(x,y) | x <- lerp l r w] | y <- lerp t b h]

render :: Image a -> Grid Coord -> Grid a
render = fmap

always :: a -> Image a
always = const

leftAndRight :: a -> a -> Image a
leftAndRight l r = \(x,y) -> if x < 0 then l else r

hypot :: Coord -> Double
hypot (x,y) = sqrt (x * x + y * y)

inCircle :: Double -> Coord -> Bool
inCircle r c = hypot c <= r

circle :: Double -> a -> a -> Image a
circle r i o = \c -> if inCircle r c then i else o

-- what we can do now is pretty cool
--
-- this converts a grid into distances from the origin
-- fmap hypot (grid (-5,-5) (5,5) 5 5)
--
-- this puts each dist in a box, separating it for readability
-- fmap ((:[]) . hypot) (grid (-5,-5) (5,5) 5 5)
--
-- or we can put quotes around each dist
-- fmap (show . hypot) (grid (-5,-5) (5,5) 5 5)
--
-- we can render out the truth of containment in a circle
-- fmap (inCircle 3) (grid (-5,-5) (5,5) 11 11)
--
-- we can render out the truth of containment in a circle
-- fmap (inCircle 3) (grid (-5,-5) (5,5) 11 11)
--
-- we can even convert that back into a Grid of Colors
-- fmap ((\x -> if x then Red else Green) . inCircle 3) (grid (-5,-5) (5,5) 11 11)
