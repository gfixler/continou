module Conal where

import Data.Monoid (mconcat, (<>))
import Color
import Image
import Interact

-- http://conal.net/pan/Gallery/interlace/cwdata/01.html
interlace01 = rings <> white
    where rings = relayer ((>0) . snd) ringL ringR
          ringL = ring . shift (2,0)
          ringR = ring . shift (-2,0)
          ring  = circle 3.1 none ( circle 3.3 white none
                                 <> circle 4.8 black none
                                 <> circle 5.0 white none )
interlace01Exp = Explorer interlace01 (-7,-5) (7,5) 120 40

-- http://conal.net/pan/Gallery/interlace/cwdata/02.html
interlace02 = rings <> white
    where rings = relayer ((>0) . snd) ringL ringR
          ringL = ring . shift (1,0)
          ringR = ring . shift (-1,0)
          ring  = circle 1.1 none (mconcat [circle r c none | (r,c) <- bw])
          bw    = zip [1.2,1.4..3] (cycle [white,black])
interlace02Exp = Explorer interlace02 (-4,-3) (4,3) 110 40

-- http://conal.net/pan/Gallery/interlace/cwdata/04.html
interlace04 = rings <> white
    where rings = relayer ((>0) . snd) ringL ringR
          ringL = ring . shift (2.65,0)
          ringR = ring . shift (-2.65,0)
          ring  = circle 1.4 none ( circle 1.5 white none
                                 <> circle 3.8 black none
                                 <> circle 4.0 white none )
interlace04Exp = Explorer interlace04 (-6.5,-4.2) (6.5,4.2) 120 38

