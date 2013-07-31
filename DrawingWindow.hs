module DrawingWindow (showDrawingWindow) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

width = 1600
height = 900

data World = World {
    playerObject :: Object
 }

data Object = Object {
    posX :: Float
  , posY :: Float
  , mass :: Float
  , acc  :: Float
 }

mkObject :: Float -> Float -> Float -> Float -> Object
mkObject x y s a = Object {
    posX = x
  , posY = y
  , mass = s
  , acc  = a
 }

type DeltaSeconds = Float

showDrawingWindow = play displayMode black 100 freshWorld renderWorld handleEvent (\_ x -> x)
       where
        displayMode :: Display
        displayMode = FullScreen (floor width, floor height)
        
freshWorld :: World
freshWorld = World {
  playerObject = mkObject 0 0 50 0
  }

renderWorld :: World -> Picture
renderWorld world = renderObject (playerObject world)
  where
    renderObject object = Color green $ buildPicture object

buildPicture object = let
            x  = posX object
            y  = posY object
            s  = mass object
            in
              Translate x y $ Circle s

handleEvent :: Event -> World -> World
handleEvent ev world = case ev of
    (EventKey key state _ (x, y)) -> case key of
        Char char -> world
        SpecialKey key -> world
        MouseButton button -> world
    (EventMotion (x, y)) -> world
