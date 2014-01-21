module Main where

import Repository (loadRepository)


main :: IO ()
main = do
    repository <- loadRepository
    forM_
    print "repository ready"
    return ()