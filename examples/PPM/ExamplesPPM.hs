module ExamplesPPM where

import Color
import Image
import PPM


-- Swirled Checkered Bands
-- Repeating horizontal and vertical color bars used as checker tiles

bands :: Int -> Color
bands = ((cycle [Red, Green, Blue, Yellow, Magenta, Cyan]) !!)

swirlBand :: (Coord -> Double) -> Coord -> Color
swirlBand f = bands . floor . abs . f

checkswirl :: Image Color
checkswirl = (checkers (swirlBand fst) (swirlBand snd)) . swirl 1.5

