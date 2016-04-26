module Conal where

import Data.Monoid (mconcat, (<>))
import Color
import Image
import Interact

-- http://conal.net/pan/Gallery/interlace/cwdata/01.html
interlace01Ring = circle 3.1 none (circle 3.3 white none <> circle 4.8 black none <> circle 5 white none)
interlace01RingL = interlace01Ring . shift (2,0)
interlace01RingR = interlace01Ring . shift (-2,0)
interlace01 = renderh (relayer ((>0) . snd) interlace01RingL interlace01RingR <> white) (grid (-7,-5) (7,5) 120 40)

-- http://conal.net/pan/Gallery/interlace/cwdata/02.html
interlace02Ring = circle 1.1 none (mconcat [circle r c none | (r,c) <- zip [1.2,1.4..3] (cycle [white,black])])
interlace02RingL = interlace02Ring . shift (-1,0)
interlace02RingR = interlace02Ring . shift (1,0)
interlace02 = renderh (relayer ((>0) . snd) interlace02RingL interlace02RingR <> const White) (grid (-4,-3) (4,3) 110 40)

-- http://conal.net/pan/Gallery/interlace/cwdata/04.html
interlace04Ring = circle 1.4 none (circle 1.5 white none <> circle 3.8 black none <> circle 4 white none)
interlace04RingL = interlace04Ring . shift (2.65,0)
interlace04RingR = interlace04Ring . shift (-2.65,0)
interlace04 = renderh (relayer ((>0) . snd) interlace04RingL interlace04RingR <> const White) (grid (-6.5,-4.2) (6.5,4.2) 120 38)

