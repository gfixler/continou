module Conal where

import Data.Monoid (mconcat, (<>))
import Color
import Image
import Interact

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

sonicRings = render (let ring = (circle 2 (const None) (circle 2.3 (const Black) (const None) <> circle 3.7 (checkers (const Red) (const Yellow)) (const None) <> circle 4 (const Black) (const None))) in (relayer (\(_,y) -> y < 0) (ring . shift (-1.5,0.5)) (ring . shift (1.5,-0.5))) <> checkers (const Green) (const Blue)) (grid (-5,-5) (5,5) 80 40)

