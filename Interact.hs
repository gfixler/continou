module Interact where

import Color
import Image
import KeyFSM

data Explorer a = Explorer { img :: Image a
                           , lb  :: Coord
                           , rt  :: Coord
                           , w   :: Int
                           , h   :: Int
                           }

instance Show a => Show (Explorer a) where
    show e = show (render (img e) (grid (lb e) (rt e) (w e) (h e)))

defColorExplorer :: Explorer Color
defColorExplorer = Explorer (circle 4 (const Red) (const White)) (-5,-5) (5,5) 25 12

explorender :: Show a => Explorer a -> IO ()
explorender e = do
    homeCursor
    putStrLn $ show $ render (img e) (grid (lb e) (rt e) (w e) (h e))
    return ()

type CoordMod = Double -> Double

modView :: CoordMod -> CoordMod -> CoordMod -> CoordMod -> Explorer a -> Explorer a
modView l b r t e = e { lb = let (x,y) = lb e in (l x,b y)
                      , rt = let (x,y) = rt e in (r x,t y) }

pan :: Double -> Explorer a -> Explorer a
pan n e = modView (+n') id (+n') id e
    where (l,_) = lb e
          (r,_) = rt e
          n' = (r - l) * n

ped :: Double -> Explorer a -> Explorer a
ped n e = modView id (+n') id (+n') e
    where (_,b) = lb e
          (_,t) = rt e
          n' = (t - b) * n

zoom :: Double -> Explorer a -> Explorer a
zoom n e = modView (const (lerp l r n))
                   (const (lerp b t n))
                   (const (lerp r l n))
                   (const (lerp t b n)) e
    where (l,b) = lb e
          (r,t) = rt e

data ExploreAction = ViewPan Double
                   | ViewPed Double
                   | Zoom Double
                   | ResetView
                   | NoAction

explorelate :: Char -> ExploreAction
explorelate 'h' = ViewPan (-0.2)
explorelate 'l' = ViewPan 0.2
explorelate 'j' = ViewPed (-0.2)
explorelate 'k' = ViewPed (0.2)
explorelate 'H' = ViewPan (-0.5)
explorelate 'L' = ViewPan 0.5
explorelate 'J' = ViewPed (-0.5)
explorelate 'K' = ViewPed (0.5)
explorelate 'i' = Zoom 0.09
explorelate 'o' = Zoom (-0.09)
explorelate 'I' = Zoom 0.2
explorelate 'O' = Zoom (-0.2)
explorelate '0' = ResetView
explorelate _   = NoAction

exploreact :: ExploreAction -> Explorer a -> Explorer a
exploreact (ViewPan n) = pan n
exploreact (ViewPed n) = ped n
exploreact (Zoom n)    = zoom n
exploreact ResetView   = \e -> e { lb = (-5,-5), rt = (5,5) }
exploreact _           = id

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

homeCursor :: IO ()
homeCursor = putStr "\ESC[0;0f"

explore :: Show a => Explorer a -> IO ()
explore e = do
    let ks = "hljkioHLJKIO0q"
    explorender e
    putStrLn $ (show $ lb e) ++ "\ESC[K\n" ++ (show $ rt e) ++ "\ESC[K"
    c <- silently (trap (`elem` ks) ("Valid keys: " ++ ks))
    if c == 'q'
        then return ()
        else explore (exploreact (explorelate c) e)

main :: Show a => Explorer a -> IO ()
main e = do
    clearScreen
    homeCursor
    explore e

