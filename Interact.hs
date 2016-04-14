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
    putStrLn "\ESC[2J\ESC[0;0f"
    putStrLn $ show $ render (img e) (grid (lb e) (rt e) (w e) (h e))
    return ()

type CoordMod = Double -> Double

modView :: CoordMod -> CoordMod -> CoordMod -> CoordMod -> Explorer a -> Explorer a
modView l b r t e = e { lb = let (x,y) = lb e in (l x,b y)
                      , rt = let (x,y) = rt e in (r x,t y) }

pan, ped :: Double -> Explorer a -> Explorer a
pan n = modView (+n) id (+n) id
ped n = modView id (+n) id (+n)

data ExploreAction = ViewPan Double
                   | ViewPed Double
                   | NoAction

explorelate :: Char -> ExploreAction
explorelate 'h' = ViewPan (-0.25)
explorelate 'l' = ViewPan 0.25
explorelate 'j' = ViewPed (-0.25)
explorelate 'k' = ViewPed (0.25)
explorelate _   = NoAction

exploreact :: ExploreAction -> Explorer a -> Explorer a
exploreact (ViewPan n) = pan n
exploreact (ViewPed n) = ped n
exploreact _           = id

main = do
    let e = defColorExplorer
        ks = "hljkq"
    explorender e
    c <- silently (trap (`elem` ks) ("valid keys: " ++ ks))
    explorender (exploreact (explorelate c) e)

