-- | This module contains the data types
--   which represent the state of the game
module Model where

screenRadius :: Float
screenRadius = 300 

turnSpeed :: Float
turnSpeed = 180.0

thrustAccel :: Float
thrustAccel = 250.0

dragCoeff :: Float
dragCoeff = 0.98

bulletSpeed :: Float
bulletSpeed = 400.0

bulletLifetime :: Float
bulletLifetime = 2.0

data Screen = Menu | Playing | Paused | GameOver | HighScores
  deriving(Eq, Show)

type Vector = ( Float , Float )
data TurnDir = TurnLeft | TurnRight | NoTurn
  deriving ( Eq , Show )

data Ship = Ship
  { shipPos :: Vector
  , shipVel :: Vector
  , heading :: Float
  , isThrusting :: Bool
  , turnDir :: TurnDir
  , shootCooldown :: Float
  , invulnTime :: Float
  } deriving ( Show )

data BulletOwner = PlayerBullet | UFOBullet
  deriving ( Eq , Show )

data Bullet = Bullet
  { bulletPos :: Vector
  , bulletVel :: Vector
  , bulletLifeTime :: Float
  , bulletOwner :: BulletOwner
  } deriving ( Show )

initialShip :: Ship
initialShip = Ship
  { shipPos = (0, 0)
  , shipVel = (0, 0)
  , heading = 0
  , isThrusting = False
  , turnDir = NoTurn
  , shootCooldown = 0
  , invulnTime = 0
  }

data GameState = GameState {
                  screen  :: Screen
                 , elapsedTime :: Float
                 , ship :: Ship
                 , bullets :: [Bullet]
                 } deriving(Show)

initialState :: GameState
initialState = GameState Menu 0 initialShip []

applyTurn :: Ship -> Float -> Ship
applyTurn s dt = case turnDir s of
  TurnLeft  -> s { heading = heading s + turnSpeed * dt }
  TurnRight -> s { heading = heading s - turnSpeed * dt }
  NoTurn    -> s

applyThrust :: Ship -> Float -> Ship
applyThrust s dt
  | isThrusting s =
      let rad = heading s * (pi / 180.0)
          ax  = sin rad * thrustAccel * dt
          ay  = cos rad * thrustAccel * dt
          (vx, vy) = shipVel s
      in s { shipVel = (vx + ax, vy + ay) }
  | otherwise = s

applyDrag :: Ship -> Float -> Ship
applyDrag s dt =
  let (vx, vy) = shipVel s
      decay    = dragCoeff ** (dt * 60.0)
  in s { shipVel = (vx * decay, vy * decay) }

wrap :: Vector -> Vector
wrap (x, y) = (wrapCoord x, wrapCoord y)
  where
    wrapCoord c
      | c > screenRadius  = c - (2 * screenRadius)
      | c < -screenRadius = c + (2 * screenRadius)
      | otherwise         = c

moveShip :: Ship -> Float -> Ship
moveShip s dt =
  let (x, y)   = shipPos s
      (vx, vy) = shipVel s
      newPos   = wrap (x + vx * dt, y + vy * dt)
  in s { shipPos = newPos }

fireBullet :: Ship -> Bullet
fireBullet s =
  let (x, y) = shipPos s
      rad    = heading s * (pi / 180.0)
      vx = sin rad * bulletSpeed
      vy = cos rad * bulletSpeed
  in Bullet
       { bulletPos      = (x, y)
       , bulletVel      = (vx, vy)
       , bulletLifeTime = bulletLifetime
       , bulletOwner    = PlayerBullet
       }

moveBullet :: Bullet -> Float -> Bullet
moveBullet b dt =
  let (x, y)   = bulletPos b
      (vx, vy) = bulletVel b
      newPos   = (x + vx * dt, y + vy * dt)
      newLife  = bulletLifeTime b - dt
  in b { bulletPos = newPos, bulletLifeTime = newLife }

updateBullets :: [Bullet] -> Float -> [Bullet]
updateBullets bs dt =
  [ moveBullet b dt | b <- bs, bulletLifeTime b - dt > 0 ]