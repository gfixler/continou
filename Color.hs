module Color where

import Data.Monoid (Monoid, mempty, mappend, (<>))

data Color = None
           | Black
           | Red
           | Green
           | Yellow
           | Blue
           | Magenta
           | Cyan
           | LtGray
           | DkGray
           | LtRed
           | LtGreen
           | LtYellow
           | LtBlue
           | LtMagenta
           | LtCyan
           | White
           deriving (Bounded,Enum,Eq)

instance Show Color where
    show = flip withBG " "

instance Monoid Color where
    mempty = None
    c `mappend` None = c
    _ `mappend` c = c

data ColorPair = ColorPair (Color, Color)

instance Show ColorPair where
    show (ColorPair (t,b)) = withBGFG t b "▄"

fgnum :: Color -> Int
fgnum None      = 39
fgnum Black     = 30
fgnum Red       = 31
fgnum Green     = 32
fgnum Yellow    = 33
fgnum Blue      = 34
fgnum Magenta   = 35
fgnum Cyan      = 36
fgnum LtGray    = 37
fgnum DkGray    = 90
fgnum LtRed     = 91
fgnum LtGreen   = 92
fgnum LtYellow  = 93
fgnum LtBlue    = 94
fgnum LtMagenta = 95
fgnum LtCyan    = 96
fgnum White     = 97

bgnum :: Color -> Int
bgnum = (+10) . fgnum

fg :: Color -> String
fg c = "\ESC[" ++ show (fgnum c) ++ "m"

bg :: Color -> String
bg c = "\ESC[" ++ show (bgnum c) ++ "m"

withFG :: Color -> String -> String
withFG c s = fg c ++ s ++ fg None

withBG :: Color -> String -> String
withBG c s = bg c ++ s ++ bg None

withBGFG :: Color -> Color -> String -> String
withBGFG bg fg = withBG bg . withFG fg

