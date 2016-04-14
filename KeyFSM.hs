module KeyFSM where

keyToCont :: Char -> IO ()
keyToCont c = putStrLn msg >> f
    where msg = "Press " ++ show c ++ " to continue..."
          f = do c' <- getChar
                 if c' == c then return () else f

keysToCont :: [Char] -> IO ()
keysToCont cs = putStrLn msg >> f
    where msg = "Press one of " ++ show cs ++ " to continue..."
          f = do c' <- getChar
                 if c' `elem` cs then return () else f

