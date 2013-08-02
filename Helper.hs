-- | This module contains helper functions.
-- Most notably the 'mkObject' function which creates
-- a new 'Object'.
module Helper where

import Types
import MPhysics

-- | Creates a new Object
-- Takes the following parameter:
-- x Position -> y Position -> mass -> x Velocity -> y Velocity
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

-- | Updates the position of an 'Object'
updateP :: Float -> Float -> Float -> Float
updateP oldP vel factor = oldP + vel * factor

-- | Updates the velocity of an 'Object'
updateV :: Float -> Float -> Float
updateV vel acc = vel + acc

-- | Introduces a deceleration of an 'Object' by calling 'calcDec'
updateA :: Float -> Float -> Float
updateA oldAcc mass = if newAcc < 0 then 0 else newAcc
                       where newAcc = calcDec (abs oldAcc) mass
