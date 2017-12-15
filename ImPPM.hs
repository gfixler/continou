module ImPPM where

import Data.Monoid ((<>))

import Color
import Image
import PPM

ring :: Double -> Double -> Image a -> Image a -> Image a
ring id od i o = circle od (circle id o i) o

outlineWeight = 1.5

ringWeight = 1.1
ringInner = 2.7
ringOuter = ringInner * ringWeight

bigRing :: Image Color
bigRing = ring ringInner ringOuter (const White) (const None)

dotWeight = 0.3
dotMinDist = 2.0
dotMaxDist = 3.9

dot :: Image Color
dot = circle dotWeight (const White) (const None)
   <> circle (dotWeight * outlineWeight) (const Black) (const None)

dots :: Double -> Image Color
dots t = mconcat [dot . shift (sin t * (dotMaxDist - dotMinDist) + dotMinDist, 0) . rot (x + t) | x <- [0,(360/11)..360]]

