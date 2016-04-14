{-# LANGUAGE DeriveFunctor #-}

module Image where

import Data.Function (on)
import Data.Monoid ((<>))
import Color

type Coord = (Double, Double)
type Image a = Coord -> a
newtype Grid a = Grid { runGrid :: [[a]] } deriving Functor

instance Show a => Show (Grid a) where
    show = unlines . map (concatMap show) . runGrid

lerp :: (Enum a, Fractional a) => a -> a -> Int -> [a]
lerp s e n = [(e - s) / n' * i + s | i <- [0..n']]
    where n' = fromIntegral (n - 1)

midCoord :: Coord -> Coord -> Coord
midCoord (x,y) (x',y') = ((x + x') / 2, (y + y') / 2)

spersolate :: (a -> a -> a) -> [a] -> [a]
spersolate f (x:y:ys) = x : f x y : spersolate f (y:ys)
spersolate _ xs = xs

grid :: Coord -> Coord -> Int -> Int -> Grid Coord
grid (l,b) (r,t) w h =
        Grid [[(x,y) | x <- lerp l r w] | y <- lerp t b h]

render :: Image a -> Grid Coord -> Grid a
render = fmap

upresY :: Grid Coord -> Grid Coord
upresY = Grid . spersolate (zipWith midCoord) . runGrid

pairBy :: (a -> a -> b) -> [a] -> [b]
pairBy f (x:y:ys) = f x y : pairBy f ys
pairBy _ _ = []

renderh :: Image Color -> Grid Coord -> Grid ColorPair
renderh im = Grid . mergeDown . runGrid . render im . upresY
    where mergeDown = pairBy (zipWith (curry ColorPair))

leftAndRight :: Image a -> Image a -> Image a
leftAndRight l r = \(x,y) -> if x < 0 then l (x,y) else r (x,y)

hypot :: Coord -> Double
hypot (x,y) = sqrt (x * x + y * y)

inCircle :: Double -> Coord -> Bool
inCircle r c = hypot c <= r

circle :: Double -> Image a -> Image a -> Image a
circle r i o = \c -> if inCircle r c then i c else o c

checkers :: Image a -> Image a -> Image a
checkers b w = \(x,y) -> if ((==) `on` (`mod` 2) . round) x y
                             then b (x,y) else w (x,y)

shift :: Coord -> Coord -> Coord
shift (u,v) (x,y) = (x+u,y+v)

rot :: Double -> Coord -> Coord
rot a (x,y) = ( x * cos a' - y * sin a',
                y * cos a' + x * sin a' )
    where a' = -a * pi / 180

rotAt :: Coord -> Double -> Coord -> Coord
rotAt (x,y) a = shift (-x,-y) . rot a . shift (x,y)

swirl :: Double -> Coord -> Coord
swirl a c = rot (hypot c**a) c

toRange :: Double -> Double -> Double -> Double
toRange l h x | l == h = l
              | h < l = toRange h l x
              | l <= x && x <= h = x
              | x > h = negate $ toRange (-h) (-l) (-x)
              | otherwise = f (o / r) * r + x
                    where f = fromIntegral . ceiling
                          o = l - x
                          r = h - l

tile :: Coord -> Coord -> Coord -> Coord
tile (l,b) (r,t) (x,y) = (x',y')
    where x' = toRange l r x
          y' = toRange t b y

stdGrid :: Grid Coord
stdGrid = grid (-5,-5) (5,5) 56 28

bigGrid :: Grid Coord
bigGrid = grid (-5,-5) (5,5) 80 40

