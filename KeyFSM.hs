module KeyFSM where

trap :: (Char -> Bool) -> String -> IO Char
trap p s = putStrLn s >> f
    where f = do c' <- getChar
                 if p c' then return c' else f

