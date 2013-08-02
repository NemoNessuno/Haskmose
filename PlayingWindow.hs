module PlayingWindow (showWindow) where

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
actualizeWorld delta world = world { 
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

updateObject :: DeltaSeconds -> Object -> Object
updateObject delta oldObject = oldObject {   posX = updateP (posX oldObject) (velX oldObject) (delta*10)
                                           , posY = updateP (posY oldObject) (velY oldObject) (delta*10)
                                           , velX = updateV (velX oldObject) (accX oldObject)
                                           , velY = updateV (velY oldObject) (accY oldObject)
                                           , accX = updateA (accX oldObject) (mass oldObject) 
                                           , accY = updateA (accY oldObject) (mass oldObject)}

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
