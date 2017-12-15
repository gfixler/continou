module Keynote where

import Data.Monoid ((<>))
import Data.Foldable (foldMap)
import Data.Tuple (swap)

import Color
import Image
import PPM


ring :: Double -> Double -> Image a -> Image a -> Image a
ring id od i o = circle od (circle id o i) o


data O = O { xy :: Coord, id :: Double, od :: Double }

o :: O -> Image Color
o (O (x, y) ri ro) = ring ri ro (const Black) (const None) . shift (x, y)

oSub :: O -> O -> Coord
oSub (O (x, y) _ _) (O (x', y') _ _) = (x' - x, y' - y)


lwgt = 0.15

ood = 0.8
oid = ood - lwgt

spc = 4

a = O (0,           -spc) oid ood
b = O (spc,         0)    oid ood
c = O (0,           0)    oid ood
d = O ( -spc,       0)    oid ood
e = O (spc * 1.375, spc)  oid ood
f = O (spc * 0.625, spc)  oid ood
g = O (0,           spc)  oid ood
h = O (-spc,        spc)  oid ood

angleTo :: Coord -> Double
angleTo = uncurry atan2 . swap

arrow :: O -> O -> Image Color
arrow a@(O c _ od) b
    = rect (od * 1.3, -lwgt/2)
           (hypot (oSub b a) - od * 1.3, lwgt/2)
           (const Black)
           (const None)
    . rot ((/ pi) $ (* 180) $ angleTo $ oSub b a)
    . shift c

graph = foldMap o [a, b, c, d, e, f, g, h]
     <> arrow a b
     <> arrow a c
     <> arrow a d
     <> arrow b e
     <> arrow b f
     <> arrow c g
     <> arrow d h

