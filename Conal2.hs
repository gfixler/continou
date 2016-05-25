module Conal where

import Data.Monoid (mconcat, (<>))
import Color
import Image
import Interact

hgrad :: Double -> Double -> [a] -> Image a
hgrad s o xs = \(x,_) -> cycle xs !! round (abs ((x + o) * s))

colors :: [Color]
colors = cycle $ [Red .. Cyan] ++ [LtRed .. LtCyan]

-- http://conal.net/pan/Gallery/interlace/cwdata/01.html
interlace01Ring = circle 3.1 (const None) (circle 3.3 (const White) (const
                             None) <> circle 4.8 (const Black) (const None)
                             <> circle 5 (const White) (const None))

interlace01 = rings <> white
    where rings = relayer ((>0) . snd) ringL ringR
          ringL = ring . shift (2,0)
          ringR = ring . shift (-2,0)
          ring = circle 3.1 none ( circle 3.3 white none
                                <> circle 4.8 black none
                                <> circle 5.0 white none )
interlace01Exp = Explorer interlace01 (-7,-5) (7,5) 120 40

-- http://conal.net/pan/Gallery/interlace/cwdata/02.html
interlace02 = rings <> white
    where rings = relayer ((>0) . snd) ringL ringR
          ringL = ring . shift (1,0)
          ringR = ring . shift (-1,0)
          wandb = cycle [white,black]
          ring = circle 1.1 none (mconcat [circle r c none | (r,c) <- zip [1.2,1.4..3] wandb])
interlace02Exp = Explorer interlace02 (-4,-3) (4,3) 110 40

-- http://conal.net/pan/Gallery/interlace/cwdata/04.html
interlace04Ring = circle 3.1 (const None) (circle 5 (const White) (const None) <> circle 4.8 (const Black) (const None) <> circle 3.3 (const White) (const None))
interlace04RingL = interlace04Ring . shift (2,0)
interlace04RingR = interlace04Ring . shift (-2,0)
interlace04 = renderh (const White <> relayer ((>0) . snd) interlace04RingL interlace04RingR) (grid (-7,-5) (7,5) 120 40)

ryring = oring 2 4 0.3
        (oring 2 4 0.6
        (checkers ltRed ltYellow)
        none
        (checkers red yellow))
        none
        black

bgring = oring 2 4 0.3
        (oring 2 4 0.6
        (checkers ltBlue ltGreen)
        none
        (checkers blue green))
        none
        black

ringies = render
         (relayer
         (\(_,y) -> y < 0)
         (bgring . shift (-1.5,-1))
         (ryring . shift (1.5,1)) <> dkGray)
         (grid (-6,-5) (6,5) 100 40)

stripewoodRing = render (oring 2 4 0.5
                        (circle 4
                        (hgrad 5 100 colors)
                        none . rot 20)
                        none
                        (hgrad 5 100 colors . rot (-20)))
                        (grid (-5,-5) (5,5) 100 50)

ringry = (circle 2 none (circle 2.3 black none <>
         circle 3.7 (checkers red yellow) none <> circle 4 black
         none))

-- sonicRings = render (let ring = (circle 2 (const None) (circle 2.3 (const
--                     Black) (const None) <> circle 3.7 (checkers (const Red)
--                     (const Yellow)) (const None) <> circle 4 (const Black)
--                     (const None))) in (relayer (\(_,y) -> y < 0) (ring
--                     . shift (-1.5,0.5)) (ring . shift (1.5,-0.5))) <>
--                     checkers (const Green) (const Blue)) (grid (-5,-5)
--                     (5,5) 80 40)

sonicRings = render ((relayer (\(_,y) -> y < 0)
            (ringry . shift (-1.5,-1))
            (ringry . shift (1.5,1)))
            <> checkers green blue)
            (grid (-6,-5) (6,5) 100 40)

check1Lt = checkers ltRed ltYellow
check1Dk = checkers red yellow
check2Lt = checkers ltBlue ltGreen
check2Dk = checkers blue green
ring1 = oring 2 4 0.3
       (oring 2 4 0.6 check1Lt none check1Dk)
       none
       black
ring2 = oring 2 4 0.3
       (oring 2 4 0.6 check2Lt none check2Dk)
       none
       black
sonicRings3D = render (relayer (\(_,y) -> y < 0)
              (ring1 . shift (-1.5,-1))
              (ring2 . shift (1.5,1))
              <> dkGray)
              (grid (-6,-5) (6,5) 100 40)

mahjongRing c n = mconcat (take n [ring r (r+1) c none | r <- [0,2..]])

mahjong5 = mahjongRing red 3
        <> mahjongRing green 3 . shift (5.6,-8.5)
        <> mahjongRing blue 3 . shift (-5.6,-8.5)
        <> mahjongRing green 3 . shift (-5.6,8.5)
        <> mahjongRing blue 3 . shift (5.6,8.5)
        <> rect (-15,-20) (15,20) ltGray none

tileView = (grid (-21,-21) (21,21) 100 50)

