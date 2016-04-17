module KeyFSM where

import System.IO ( BufferMode(NoBuffering)
                 , hSetBuffering, hSetEcho
                 , hGetBuffering, hGetEcho
                 , stdin, stdout, getChar
                 )

silently :: IO a -> IO a
silently f = do
    inB <- hGetBuffering stdin
    outB <- hGetBuffering stdout
    inE <- hGetEcho stdin
    outE <- hGetEcho stdout
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hSetEcho stdout True -- HACK: required for clean exit
    hSetEcho stdin False
    hSetEcho stdout False
    r <- f
    hSetBuffering stdin inB
    hSetBuffering stdout outB
    hSetEcho stdin inE
    hSetEcho stdout outE
    return r

trap :: (Char -> Bool) -> String -> IO Char
trap p s = putStrLn s >> f
    where f = do c' <- getChar
                 if p c' then return c' else f

