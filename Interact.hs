module Interact where

import Color
import Image

data Explorer a = Explorer { img :: Image a
                           , lb  :: Coord
                           , rt  :: Coord
                           , w   :: Int
                           , h   :: Int
                           }

instance Show a => Show (Explorer a) where
    show e = show (render (img e) (grid (lb e) (rt e) (w e) (h e)))

defColorExplorer :: Explorer Color
defColorExplorer = Explorer (const White) (-5,-5) (5,5) 21 10

explorender :: Show a => Explorer a -> IO ()
explorender e = do
    putStrLn "\ESC[2J\ESC[0;0f"
    putStrLn $ show $ render (img e) (grid (lb e) (rt e) (w e) (h e))
    return ()

viewLeft :: Double -> Explorer a -> Explorer a
viewLeft n e = e { lb = let (x,y) = lb e in (x-n,y)
                 , rt = let (x,y) = rt e in (x-n,y) }

viewRight:: Double -> Explorer a -> Explorer a
viewRight n e = e { lb = let (x,y) = lb e in (x+n,y)
                  , rt = let (x,y) = rt e in (x+n,y) }

viewUp:: Double -> Explorer a -> Explorer a
viewUp n e = e { lb = let (x,y) = lb e in (x,y+n)
               , rt = let (x,y) = rt e in (x,y+n) }

viewDown:: Double -> Explorer a -> Explorer a
viewDown n e = e { lb = let (x,y) = lb e in (x,y-n)
                 , rt = let (x,y) = rt e in (x,y-n) }

