module PlayingWindow (showWindow) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import MPhysics

import Types
import Helper

width = 1600
height = 900
basePower = 50

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
actualizeWorld delta world = world { playerObject = updateObject (playerObject world) delta}

renderWorld :: World -> Picture
renderWorld world = Pictures ((renderPlayer (playerObject world)) : (map (renderObject (mass (playerObject world))) (otherObjects world)))
  where
    renderPlayer player = Color green $ buildPicture player
    renderObject playerMass object = Color oColor $ buildPicture object
      where
        oColor = if (playerMass >= (mass object)) then blue else red

updateObject :: Object -> DeltaSeconds -> Object
updateObject oldObject delta = oldObject {   posX = updateP (posX oldObject) (velX oldObject) delta
                                           , posY = updateP (posY oldObject) (velY oldObject) delta
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
        SpecialKey key -> world { playerObject = playerObject'} 
          where playerObject' = case state of
                                  Down -> case key of
                                    KeyUp    -> (playerObject world) { accY = (accY (playerObject world)) + (calcAcc basePower (mass (playerObject world)))}
                                    KeyDown  -> (playerObject world) { accY = (accY (playerObject world)) - (calcAcc basePower (mass (playerObject world)))}
                                    KeyRight -> (playerObject world) { accX = (accX (playerObject world)) + (calcAcc basePower (mass (playerObject world)))}
                                    KeyLeft  -> (playerObject world) { accX = (accX (playerObject world)) - (calcAcc basePower (mass (playerObject world)))}
                                    _        -> (playerObject world)
                                  Up   -> case key of
                                    _        -> (playerObject world)
        MouseButton button -> world
    (EventMotion (x, y)) -> world
