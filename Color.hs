data Color = Black
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
           deriving (Bounded,Enum,Eq,Show)

fgnum :: Color -> Int
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

