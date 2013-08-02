-- | This module contains helper functions.
-- Most notably the 'mkObject' function which creates
-- a new 'Object'.
module Helper where

import Types
import MPhysics

-- | The mass that is ejected. And the force it causes.
basePower :: Float
basePower = 2.0

-- | Creates a new 'Object'
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

-- | Creates a new 'Object'
-- Takes the following parameter:
-- x Position -> y Position -> mass -> x Acceleration -> y Acceleration
mkObject' :: Float -> Float -> Float -> Float -> Float ->  Object
mkObject' x y s ax ay = Object {
    posX = x
  , posY = y
  , mass = s
  , accX = ax
  , accY = ay
  , velX = 0
  , velY = 0
}

-- |Calculates the new mass after ejecting a new 'Object' with the
-- mass of 'basePower'
ejectMass :: Object -> Float
ejectMass o = (mass o)-basePower

-- | Copies an 'Object' by positioning the copy it below the center
-- of the copied object. The copy gets the mass of 'basePower'
-- and an acceleration equal to the negative of the acceleration
-- the copied object got.
copyB :: Object -> Object
copyB o = mkObject' (posX o) ((posY o) - (mass o) - basePower) basePower 0 (calcAcc (-basePower / ((mass o) / 2)) basePower)

-- | Copies an 'Object' by positioning the copy it above the center
-- of the copied object. The copy gets the mass of 'basePower'
-- and an acceleration equal to the negative of the acceleration
-- the copied object got.
copyA :: Object -> Object
copyA o = mkObject' (posX o) ((posY o) + (mass o) + basePower) basePower 0 (calcAcc ( basePower / ((mass o) / 2)) basePower)

-- | Copies an 'Object' by positioning the copy left of the center
-- of the copied object. The copy gets the mass of 'basePower'
-- and an acceleration equal to the negative of the acceleration
-- the copied object got.
copyL :: Object -> Object
copyL o = mkObject' (posX o - (mass o) - basePower) (posY o) basePower (calcAcc (-basePower / ((mass o) / 2)) basePower) 0

-- | Copies an 'Object' by positioning the copy right of the center
-- of the copied object. The copy gets the mass of 'basePower'
-- and an acceleration equal to the negative of the acceleration
-- the copied object got.
copyR :: Object -> Object
copyR o = mkObject' (posX o + (mass o) + basePower) (posY o) basePower (calcAcc (basePower / ((mass o) / 2)) basePower) 0

-- | Applies a certain binary Function to a the current acc of an 'Object' 
applyAcc :: (Float -> Float -> Float) -> Float -> Object -> Float
applyAcc f oldAcc object = oldAcc `f` (calcAcc basePower (mass object))

-- | Updates the position of an 'Object'
updateP :: Float -> Float -> Float -> Float
updateP oldP vel factor = oldP + vel * factor

-- | Updates the velocity of an 'Object'
updateV :: Float -> Float -> Float
updateV vel acc = vel + acc

-- | Introduces a deceleration of an 'Object' by calling 'calcDec'
updateA :: Float -> Float -> Float
updateA oldAcc mass = if newAcc < 0 then 0 else newAcc * factor
                       where 
                        newAcc = calcDec (abs oldAcc) mass
                        factor = if (oldAcc < 0) then -1 else 1
