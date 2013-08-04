-- | This module contains helper functions.
-- Most notably the 'mkObject' function which creates
-- a new 'Object'.
module Helper(mkObject, copyB, copyL, copyA, copyR, updateA, updateP, updateV, ejectMass, applyAcc, consumeList, consumeEach) where

import Types
import MPhysics

-- * Public

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

-- |Calculates the new mass after ejecting a new 'Object' with the
-- mass of 'basePower'
ejectMass :: Object -> Float
ejectMass o = (mass o)-basePower

-- | Applies a certain binary Function to a the current acc of an 'Object' 
applyAcc :: (Float -> Float -> Float) -> Float -> Object -> Float
applyAcc f oldAcc object = oldAcc `f` (calcAcc basePower (mass object))

-- | Checks if an 'Object' consumes an 'Object' from a List and returns 
-- a tuple of the new List and the new 'Object'. If the single Object
-- is consumed it returns 'Invalid'. The returned List only
-- contains valid 'Object's.
consumeList :: (Object, [Object]) -> (Object, [Object])
consumeList (object, [])        = (object, [])
consumeList (Invalid, os)       = (Invalid, os)

consumeList (object, [oObject]) = let (object', oObject') = checkConsumation object oObject in
                                    case oObject' of
                                      Invalid   -> (object', [])
                                      otherwise -> (object' , [oObject'])

consumeList (object, o:os)      = (object', o' ++ os')
                                    where
                                      (object'', os') = consumeList (object, os)
                                      (object', o')   = case object'' of
                                                         Invalid   -> (Invalid, [o])
                                                         otherwise -> consumeList (object'', [o])

-- | Checks every Object in the List for consumation
consumeEach :: [Object] -> [Object]
consumeEach []     = []
consumeEach [o]    = [o]
consumeEach (o:os) = let (newO, newOs) = consumeList (o, os) in
                      case newO of
                        Invalid   -> consumeEach newOs
                        otherwise -> newO : consumeEach newOs

-- * Private

-- | The mass that is ejected. And the force it causes.
basePower :: Float
basePower = 2.0

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

-- | Checks if two 'Object's collide. Uses good ol' Pythagoras to determine
-- the distance of two points
collide :: Object -> Object -> Bool
collide o oO = (pytho (px,py) (px',py')) <= (r + r')
              where 
                px  = (posX o)
                py  = (posY o)
                px' = (posX oO)
                py' = (posY oO)
                r  = (mass o)
                r' = (mass oO)
                pytho (x, y) (x', y') = sqrt ((y-y')^2 + (x-x')^2)
                                                        
-- | Checks if two 'Object's collide and if they do returns a tuple
-- where the smaller 'Object' is rendered invalid and the larger
-- ones mass is the sum of the mass of both of them.
checkConsumation :: Object -> Object -> (Object, Object)
checkConsumation object oObject = if collide object oObject then
                                      if (mass object) >= (mass oObject) then
                                          (consumeObject object oObject, Invalid)
                                        else
                                          (Invalid, consumeObject oObject object)
                                    else
                                      (object, oObject)

-- | Returns an 'Object' {mass = (mass o1 + mass o2)}               
consumeObject :: Object -> Object -> Object
consumeObject o oO = o {mass = m1 + m2}
                    where
                      m1 = mass o
                      m2 = mass oO
