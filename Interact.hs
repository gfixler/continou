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
    putStrLn "\ESC[H\ESC[0;0f"
    putStrLn $ show $ render (img e) (grid (lb e) (rt e) (w e) (h e))
    return ()

