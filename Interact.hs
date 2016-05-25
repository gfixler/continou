module Interact where

import Color
import Image
import KeyFSM

eraseToEOL :: IO ()
eraseToEOL = putStr "\ESC[K"

clearScrn :: IO ()
clearScrn = putStr "\ESC[2J"

posCursor :: Int -> Int -> IO ()
posCursor x y = putStr ("\ESC[" ++ show x ++ ";" ++ show y ++ "f")

homeCursor :: IO ()
homeCursor = posCursor 0 0

newline :: IO ()
newline = putStrLn ""

data View a = View { lb  :: Coord
                   , rt  :: Coord
                   , w   :: Int
                   , h   :: Int }

explorender :: Show a => Image a -> View a -> IO ()
explorender i v = do
    homeCursor
    print $ render i (grid (lb v) (rt v) (w v) (h v))
    return ()

type CoordMod = Double -> Double

modView :: CoordMod -> CoordMod -> CoordMod -> CoordMod -> View a -> View a
modView l b r t v = v { lb = let (x,y) = lb v in (l x,b y)
                      , rt = let (x,y) = rt v in (r x,t y) }

pan :: Double -> View a -> View a
pan n v = modView (+n') id (+n') id v
    where (l,_) = lb v
          (r,_) = rt v
          n' = (r - l) * n

ped :: Double -> View a -> View a
ped n v = modView id (+n') id (+n') v
    where (_,b) = lb v
          (_,t) = rt v
          n' = (t - b) * n

zoom :: Double -> View a -> View a
zoom n v = modView (const (lerp l r n))
                   (const (lerp b t n))
                   (const (lerp r l n))
                   (const (lerp t b n)) v
    where (l,b) = lb v
          (r,t) = rt v

data ExploreAction = ViewPan Double
                   | ViewPed Double
                   | Zoom Double
                   | ResetView
                   | Times Int ExploreAction
                   | NoAction

explorelate :: Char -> ExploreAction
explorelate 'h' = ViewPan (-0.2)
explorelate 'l' = ViewPan 0.2
explorelate 'j' = ViewPed (-0.2)
explorelate 'k' = ViewPed (0.2)
explorelate 'H' = Times 2 (ViewPan (-0.2))
explorelate 'L' = Times 2 (ViewPan 0.2)
explorelate 'J' = Times 2 (ViewPed (-0.2))
explorelate 'K' = Times 2 (ViewPed (0.2))
explorelate 'i' = Zoom (1/10)
explorelate 'o' = Zoom (-1/8)
explorelate 'I' = Times 3 (Zoom (1/10))
explorelate 'O' = Times 3 (Zoom (-1/8))
explorelate '0' = ResetView
explorelate _   = NoAction

exploreview :: ExploreAction -> View a -> View a
exploreview (ViewPan n) = pan n
exploreview (ViewPed n) = ped n
exploreview (Zoom n)    = zoom n
exploreview (Times t a) | t < 1 = id
                       | otherwise = exploreview (Times (t-1) a)
                       . exploreview a
exploreview ResetView   = \v -> v { lb = (-5,-5), rt = (5,5) }
exploreview _           = id

eolStrLn :: String -> IO ()
eolStrLn s = do
    putStr s
    eraseToEOL
    newline

explore :: Show a => Image a -> View a -> IO ()
explore i v = clearScrn >> do
    let ks = "hljkioHLJKIO0q"
    explorender i v
    eolStrLn $ show (lb v)
    eolStrLn $ show (rt v)
    c <- silently (trap (`elem` ks) ("Valid keys: " ++ ks))
    if c == 'q'
        then return ()
        else explore i (exploreview (explorelate c) v)

