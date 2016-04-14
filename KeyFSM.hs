module KeyFSM where

import System.IO ( BufferMode(NoBuffering)
                 , hSetBuffering, hSetEcho
                 , hGetBuffering, hGetEcho
                 , stdin, stdout, getChar
                 )

trap :: (Char -> Bool) -> String -> IO Char
trap p s = putStrLn s >> f
    where f = do c' <- getChar
                 if p c' then return c' else f

