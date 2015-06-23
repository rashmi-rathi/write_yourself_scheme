import System.Environment

main = do
    args <- getArgs
    let x = read (args !! 0)::Int
        y = read (args !! 1)::Int
        in putStrLn . show  $ (x + y)
