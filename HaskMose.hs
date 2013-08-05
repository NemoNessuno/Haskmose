module Main where

import System.Environment

import PlayingWindow
import LevelBuilder
import LevelParser

main = do
        args <- getArgs
        case args of
          {-[width, height] -> showWindow createRandom (read width) (read height)-}
          [fileName] -> do 
                          objects <- createFromFile fileName
                          showWindow objects
          _          -> showWindow createRandom
