-- | The "Types" modules houses all types which are Used by HaskMose
module Types where

-- | An 'Object' which models every cell in the game
data Object = Object {
    posX :: Float
  , posY :: Float
  , mass :: Float
  , accX :: Float
  , accY :: Float
  , velX :: Float
  , velY :: Float
}

