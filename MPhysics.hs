-- | The "MPhysics" module contains all functions needed to 
-- model the physical properties of the HaskMoses world
-- (i.e. acceleration and decelaration)
module MPhysics where

-- |Simple calculation of the acceleration
-- |of our bodies. We of course ignore frictioni
-- |Returns new acceleration
calcAcc :: Float -> Float -> Float
calcAcc power mass = power / mass

-- |Since we ignore friction this calculation
-- |isn't linked to the real world in any way
-- |We just say that the body decelarates by
-- |1/100 of its mass.
-- |Returns new acceleration
calcDec :: Float -> Float -> Float
calcDec oldAcc mass = oldAcc - mass/100
