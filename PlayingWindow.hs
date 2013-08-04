module PlayingWindow {-(showWindow)-} where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import MPhysics

import Types
import Helper

width = 1600
height = 900

data World = World {
    playerObject :: Object
  , otherObjects :: [Object]
 }

type DeltaSeconds = Float

showWindow otherCells = play displayMode black 100 (freshWorld otherCells) renderWorld handleEvent actualizeWorld
       where
        displayMode :: Display
        displayMode = FullScreen (floor width, floor height)
        
freshWorld :: [Object] -> World
freshWorld otherCells = World {
  playerObject = mkObject 0 0 50 0 0
, otherObjects = otherCells
  }

actualizeWorld :: DeltaSeconds -> World -> World
actualizeWorld delta world = consumeObjects world { 
                              playerObject = updateObject delta (playerObject world)
                            , otherObjects = map (updateObject delta) (otherObjects world)
                            }

renderWorld :: World -> Picture
renderWorld world = Pictures ((renderPlayer (playerObject world)) : (map (renderObject (mass (playerObject world))) (otherObjects world)))
  where
    renderPlayer player = Color green $ buildPicture player
    renderObject playerMass object = Color oColor $ buildPicture object
      where
        oColor = if (playerMass >= (mass object)) then blue else red

-- | Takes care of the consumation of 'Object's. Applies the neccessary changes to the World.
consumeObjects :: World -> World
consumeObjects world = World { playerObject = pO', otherObjects = oO' }
                       where (pO', oO') = consumeList ((playerObject world), (otherObjects world))
                       

-- | Updates the position, velocity and acceleration of an object
updateObject :: DeltaSeconds -> Object -> Object
updateObject delta oldObject = oldObject {   posX = updateP (posX oldObject) (velX oldObject) (delta*50*dirX)
                                           , posY = updateP (posY oldObject) (velY oldObject) (delta*50*dirY)
                                           , velX = (updateV (velX oldObject) (accX oldObject)) * dirX
                                           , velY = (updateV (velY oldObject) (accY oldObject)) * dirY
                                           , accX = (updateA (accX oldObject) (mass oldObject)) * dirX
                                           , accY = (updateA (accY oldObject) (mass oldObject)) * dirY}
                               where
                                dirX = if (abs (posX oldObject)) + (mass oldObject) >= width / 2 then -1 else 1
                                dirY = if (abs (posY oldObject)) + (mass oldObject) >= height / 2 then -1 else 1

-- | Draws an Object
buildPicture :: Object -> Picture
buildPicture object = let
            x  = posX object
            y  = posY object
            s  = mass object
            in
              Translate x y $ Circle s

-- | Event Handler to play.
-- Listens fo the direction keys and applies accelaration in the chosen direction.
-- Also triggers the ejection of mass as response.
handleEvent :: Event -> World -> World
handleEvent ev world = case ev of
    (EventKey key state _ (x, y)) -> case key of
        Char char -> world
        SpecialKey key -> world { playerObject = playerObject', otherObjects = otherObjects'} 
          where (playerObject', otherObjects') = case state of
                                  Down -> case key of
                                    KeyUp    -> 
                                      ((playerObject world) { 
                                          accY = applyAcc (+) (accY (playerObject world)) (playerObject world)
                                        , mass = ejectMass (playerObject world)
                                      },
                                      copyB (playerObject world) : (otherObjects world))
                                    KeyDown  -> 
                                      ((playerObject world) { 
                                          accY = applyAcc (-) (accY (playerObject world)) (playerObject world) 
                                        , mass = ejectMass (playerObject world)
                                      },
                                      copyA (playerObject world) : (otherObjects world))
                                    KeyRight -> ((playerObject world) { 
                                          accX = applyAcc (+) (accX (playerObject world)) (playerObject world)
                                        , mass = ejectMass (playerObject world)
                                      }, 
                                      copyL (playerObject world) : (otherObjects world))
                                    KeyLeft  -> ((playerObject world) {
                                          accX = applyAcc (-) (accX (playerObject world)) (playerObject world)
                                        , mass = ejectMass (playerObject world)
                                      }, 
                                      copyR (playerObject world) : (otherObjects world))
                                    _        -> ((playerObject world), (otherObjects world))
                                  Up   -> case key of
                                    _        -> ((playerObject world), (otherObjects world))
        MouseButton button -> world
    (EventMotion (x, y)) -> world
