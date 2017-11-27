import qualified Data.Map as M (Map, fromList, (!))

import Color

type PPMCol = (Int, Int, Int)

newtype PPMColors = PPMColors { getPPMColors :: M.Map Color PPMCol }
                    deriving (Eq, Ord, Show)

getColor :: PPMColors -> Color -> PPMCol
getColor cs = ((M.!) (getPPMColors cs))

vga = PPMColors $ M.fromList
    [ (None,       (0,0,0))
    , (Black,      (0,0,0))
    , (Red,        (170,0,0))
    , (Green,      (0,170,0))
    , (Yellow,     (170,85,0))
    , (Blue,       (0,0,170))
    , (Magenta,    (170,0,170))
    , (Cyan,       (0,170,170))
    , (LtGray,     (170,170,170))
    , (DkGray,     (85,85,85))
    , (LtRed,      (255,85,85))
    , (LtGreen,    (85,255,85))
    , (LtYellow,   (255,255,85))
    , (LtBlue,     (85,85,255))
    , (LtMagenta,  (255,85,255))
    , (LtCyan,     (85,255,255))
    , (White,      (255,255,255)) ]

cmd = PPMColors $ M.fromList
    [ (None,       (0,0,0))
    , (Black,      (0,0,0))
    , (Red,        (128,0,0))
    , (Green,      (0,128,0))
    , (Yellow,     (128,128,0))
    , (Blue,       (0,0,128))
    , (Magenta,    (128,0,128))
    , (Cyan,       (0,128,128))
    , (LtGray,     (192,192,192))
    , (DkGray,     (128,128,128))
    , (LtRed,      (255,0,0))
    , (LtGreen,    (0,255,0))
    , (LtYellow,   (255,255,0))
    , (LtBlue,     (0,0,255))
    , (LtMagenta,  (255,0,255))
    , (LtCyan,     (0,255,255))
    , (White,      (255,255,255)) ]

terminalapp = PPMColors $ M.fromList
    [ (None,       (0,0,0))
    , (Black,      (0,0,0))
    , (Red,        (194,54,33))
    , (Green,      (37,188,36))
    , (Yellow,     (173,173,39))
    , (Blue,       (73,46,225))
    , (Magenta,    (211,56,211))
    , (Cyan,       (51,187,200))
    , (LtGray,     (203,204,205))
    , (DkGray,     (129,131,131))
    , (LtRed,      (252,57,31))
    , (LtGreen,    (49,231,34))
    , (LtYellow,   (234,236,35))
    , (LtBlue,     (88,51,255))
    , (LtMagenta,  (249,53,248))
    , (LtCyan,     (20,240,240))
    , (White,      (233,235,235)) ]

putty = PPMColors $ M.fromList
    [ (None,       (0,0,0))
    , (Black,      (0,0,0))
    , (Red,        (187,0,0))
    , (Green,      (0,187,0))
    , (Yellow,     (187,187,0))
    , (Blue,       (0,0,187))
    , (Magenta,    (187,0,187))
    , (Cyan,       (0,187,187))
    , (LtGray,     (187,187,187))
    , (DkGray,     (85,85,85))
    , (LtRed,      (255,85,85))
    , (LtGreen,    (85,255,85))
    , (LtYellow,   (255,255,85))
    , (LtBlue,     (85,85,255))
    , (LtMagenta,  (255,85,255))
    , (LtCyan,     (85,255,255))
    , (White,      (255,255,255)) ]

mirc = PPMColors $ M.fromList
    [ (None,       (0,0,0))
    , (Black,      (0,0,0))
    , (Red,        (127,0,0))
    , (Green,      (0,147,0))
    , (Yellow,     (252,127,0))
    , (Blue,       (0,0,127))
    , (Magenta,    (156,0,156))
    , (Cyan,       (0,147,147))
    , (LtGray,     (210,210,210))
    , (DkGray,     (127,127,127))
    , (LtRed,      (255,0,0))
    , (LtGreen,    (0,252,0))
    , (LtYellow,   (255,255,0))
    , (LtBlue,     (0,0,252))
    , (LtMagenta,  (255,0,255))
    , (LtCyan,     (0,255,255))
    , (White,      (255,255,255)) ]

xterm = PPMColors $ M.fromList
    [ (None,       (0,0,0))
    , (Black,      (0,0,0))
    , (Red,        (205,0,0))
    , (Green,      (0,205,0))
    , (Yellow,     (205,205,0))
    , (Blue,       (0,0,238))
    , (Magenta,    (205,0,205))
    , (Cyan,       (0,205,205))
    , (LtGray,     (229,229,229))
    , (DkGray,     (127,127,127))
    , (LtRed,      (255,0,0))
    , (LtGreen,    (0,255,0))
    , (LtYellow,   (255,255,0))
    , (LtBlue,     (92,92,255))
    , (LtMagenta,  (255,0,255))
    , (LtCyan,     (0,255,255))
    , (White,      (255,255,255)) ]

ubuntu = PPMColors $ M.fromList
    [ (None,       (0,0,0))
    , (Black,      (1,1,1))
    , (Red,        (222,56,43))
    , (Green,      (57,181,74))
    , (Yellow,     (255,199,6))
    , (Blue,       (0,111,184))
    , (Magenta,    (118,38,113))
    , (Cyan,       (44,181,233))
    , (LtGray,     (204,204,204))
    , (DkGray,     (128,128,128))
    , (LtRed,      (255,0,0))
    , (LtGreen,    (0,255,0))
    , (LtYellow,   (255,255,0))
    , (LtBlue,     (0,0,255))
    , (LtMagenta,  (255,0,255))
    , (LtCyan,     (0,255,255))
    , (White,      (255,255,255)) ]

