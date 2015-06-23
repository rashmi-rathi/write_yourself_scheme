import System.Environment


main = do
    args <- getArgs
    putStrLn( "Hello , " ++ args !! 0 ++  " and "++ args !! 1)
