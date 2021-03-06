{-# LANGUAGE DeriveFunctor #-}

module Image where

import Data.Complex (Complex((:+)), magnitude)
import Data.Fixed (mod')
import Data.Function (on)
import Data.Monoid (Monoid, (<>))
import Color

type Coord = (Double, Double)
type Image a = Coord -> a
newtype Grid a = Grid { runGrid :: [[a]] } deriving Functor

instance Show a => Show (Grid a) where
    show = unlines . map (concatMap show) . runGrid

lerp :: Fractional a => a -> a -> a -> a
lerp a b t = (b - a) * t + a

lerps :: (Enum a, Fractional a) => a -> a -> Int -> [a]
lerps a b n = map (lerp a b) [0,1/n'..1]
    where n' = fromIntegral (n-1)

midCoord :: Coord -> Coord -> Coord
midCoord (x,y) (x',y') = ((x + x') / 2, (y + y') / 2)

spersolate :: (a -> a -> a) -> [a] -> [a]
spersolate f (x:y:ys) = x : f x y : spersolate f (y:ys)
spersolate _ xs = xs

grid :: Coord -> Coord -> Int -> Int -> Grid Coord
grid (l,b) (r,t) w h =
        Grid [[(x,y) | x <- lerps l r w] | y <- lerps t b h]

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

between :: Ord a => a -> a -> a -> Bool
between a b x = a <= x && x <= b || b <= x && x <= a

inRect :: Coord -> Coord -> Coord -> Bool
inRect (l,b) (r,t) (x,y) = between l r x && between b t y

rect :: Coord -> Coord -> Image a -> Image a -> Image a
rect lb rt i o = \c -> if inRect lb rt c then i c else o c

hypot :: Coord -> Double
hypot (x,y) = sqrt (x * x + y * y)

inCircle :: Double -> Coord -> Bool
inCircle r c = hypot c <= r

circle :: Double -> Image a -> Image a -> Image a
circle r i o = \c -> if inCircle r c then i c else o c

checkers :: Image a -> Image a -> Image a
checkers b w = \(x,y) -> if ((==) `on` (`mod` 2) . round) x y
                             then b (x,y) else w (x,y)

relayer :: Monoid a => (Coord -> Bool) -> Image a -> Image a -> Image a
relayer p t f = \c -> if p c then ((t <> f) c) else ((f <> t) c)

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
toRange a b x = ((x-a) `mod'` (b-a)) + a

tile :: Coord -> Coord -> Coord -> Coord
tile (l,b) (r,t) (x,y) = (x',y')
    where x' = toRange l r x
          y' = toRange t b y

intToColor :: Int -> Color
intToColor = ((cycle [succ $ succ minBound .. maxBound]) !!)

mandelbrot :: RealFloat a => b -> (Int -> b) -> Int -> (a, a) -> b
mandelbrot x f l (r,i) = if n == l then x else f n
    where ms = iterate (\z -> z^2 + (r :+ i) + 1) 0
          n  = length $ take l $ takeWhile ((<2) . magnitude) ms

mandelbrotColor :: Int -> Coord -> Color
mandelbrotColor = mandelbrot None intToColor

sqrid :: Double -> Int -> Grid Coord
sqrid iw rw = grid (-iw/2,-iw/2) (iw/2,iw/2) rw (rw `div` 2)

