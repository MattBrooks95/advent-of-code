module Day22 (
    run
    ) where

import System.FilePath (
    FilePath
    )

run :: FilePath -> IO ()
run fp = do
    putStrLn $ "day22 - " <> fp
