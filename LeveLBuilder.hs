-- | The "LevelBuilder" modules encapsulates several ways to create Levels.
module LevelBuilder(createRandom) where

import Types
import Helper

-- | Creates a Level by returning a list of randomly
-- generated 'Objects'
-- TODO: Needs more code (i.e. count of cells?, also generate Player position?, use real randomness)
createRandom :: [Object]
createRandom = [mkObject 100 100 20 0 0, mkObject (-100) (-100) 60 0 0] 
