{-# LANGUAGE DeriveFunctor #-}

module Image where

import Data.Complex (Complex((:+)), magnitude)
import Data.Fixed (mod')
import Data.Function (on)
import Data.List (minimumBy)
import Data.Monoid (Monoid, (<>))
import Data.Ord (comparing)
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

ring :: Double -> Double -> Image a -> Image a -> Image a
ring ir or i o = circle ir o (circle or i o)

checkers :: Image a -> Image a -> Image a
checkers b w = \(x,y) -> if ((==) `on` (`mod` 2) . round) x y
                             then b (x,y) else w (x,y)

sine :: Double -> Double -> Image a -> Image a -> Image a
sine a w b t = \(x,y) -> if y <= a * sin (x * w) then b (x,y) else t (x,y)

angle :: Coord -> Double
angle (x,y) = case signum a of (-1) -> pi + a + pi
                               _    -> a
    where a = atan2 y x

rerange :: Fractional a => a -> a -> a -> a -> a -> a
rerange a b a' b' x = ((x - a) / (b - a)) * (b' - a') + a'

radials :: Num a => [a] -> Image a
radials xs = (xs !!) . index
    where index = floor . inRange . angle
          inRange = rerange 0 (2*pi) 0 listlen
          listlen = fromIntegral $ length xs

dist :: Coord -> Coord -> Double
dist (x,y) (u,v) = sqrt ((x-u)*(x-u)+(y-v)*(y-v))

voronoi :: [(Coord,Image a)] -> Image a
voronoi cas = \c -> (snd $ minimumBy (comparing (dist c . fst)) cas) c

quadBez :: Coord -> Coord -> Coord -> Int -> [Coord]
quadBez (ax,ay) (bx,by) (cx,cy) n = zip xs ys
    where n' = fromIntegral n
          ts = [0,1/n'..1]
          xs = [lerp (lerp ax bx t) (lerp bx cx t) t | t <- ts]
          ys = [lerp (lerp ay by t) (lerp by cy t) t | t <- ts]

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

type Cloud = [(Double, Coord)]

inCloud :: Cloud -> Image Bool
inCloud cs = \c -> foldr (\(r,p) -> (|| inCircle r (shift p c))) False cs

cloud :: Cloud -> (Double -> Double) -> (Coord -> Coord) -> Image a -> Image a -> Image a
cloud cs f g i o = \xy -> if (foldr (\(r,c) -> (|| inCircle r (shift c xy))) False cs')
                           then i xy else o xy
    where cs' = map (\(r,c) -> (f r, g c)) cs

blob :: Cloud
blob = [(1,(-2,-2)),(3,(0,1)),(1,(1,-2))]

brain = [ (79.924,(263.56,144.625))
        , (98.954,(334.445,155.091))
        , (119.886,(90.866,170.791))
        , (137.964,(297.813,156.043))
        , (144.149,(89.439,170.791))
        , (161.276,(272.317,159.752))
        , (162.227,(94.672,171.266))
        , (162.227,(107.041,184.587))
        , (162.227,(123.692,195.053))
        , (162.227,(142.722,203.141))
        , (162.227,(165.557,209.325))
        , (162.227,(186.966,211.704))
        , (165.557,(163.179,141.77))
        , (176.024,(253.569,162.227))
        , (190.296,(235.491,161.752))
        , (206.471,(210.85,157.946))
        , (208.85,(179.354,164.606))
        , (208.85,(194.578,162.227))
        ]

