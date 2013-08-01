module PlayingWindow (showWindow) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import MPhysics

width = 1600
height = 900
basePower = 10

data World = World {
    playerObject :: Object
 }

data Object = Object {
    posX :: Float
  , posY :: Float
  , mass :: Float
  , accX :: Float
  , accY :: Float
  , velX :: Float
  , velY :: Float
 }

mkObject :: Float -> Float -> Float -> Float -> Float ->  Object
mkObject x y s vx vy = Object {
    posX = x
  , posY = y 
  , mass = s
  , accX = 0
  , accY = 0
  , velX = vx
  , velY = vy
 }

type DeltaSeconds = Float

showWindow = play displayMode black 100 freshWorld renderWorld handleEvent actualizeWorld
       where
        displayMode :: Display
        displayMode = FullScreen (floor width, floor height)
        
freshWorld :: World
freshWorld = World {
  playerObject = mkObject 0 0 50 0 0
  }

actualizeWorld :: DeltaSeconds -> World -> World
actualizeWorld delta world = world { playerObject = updateObject (playerObject world) delta}

renderWorld :: World -> Picture
renderWorld world = renderObject (playerObject world)
  where
    renderObject object = Color green $ buildPicture object

updateObject :: Object -> DeltaSeconds -> Object
updateObject oldObject delta = oldObject {   posX = updateF (posX oldObject) (velX oldObject) delta
                                           , posY = updateF (posY oldObject) (velY oldObject) delta
                                           , velX = updateF (velX oldObject) (accX oldObject) 1
                                           , velY = updateF (velY oldObject) (accY oldObject) 1}

updateF :: Float -> Float -> Float -> Float
updateF sum1 sum2 factor = sum1 + sum2 * factor

buildPicture :: Object -> Picture
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
        SpecialKey key -> world { playerObject = playerObject'} 
          where playerObject' = case state of
                                  Down -> case key of
                                    KeyUp   -> (playerObject world) { accY = (accY (playerObject world)) + (calcAcc basePower (mass (playerObject world))) }
                                    KeyDown -> (playerObject world) { accY = (accY (playerObject world)) - (calcAcc basePower (mass (playerObject world))) }
                                    _       -> (playerObject world)
                                  Up   -> case key of
                                    _       -> (playerObject world)
        MouseButton button -> world
    (EventMotion (x, y)) -> world
