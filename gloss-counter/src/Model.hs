-- | This module contains the data types
--   which represent the state of the game
module Model where

-- TODO
-- [ ] Make astroids move from start
-- [ ] Score counting
-- [ ] High score page
-- [ ] Store scores to computer
-- [ ] Add space ships

import System.Random (StdGen, randomR)

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

data AsteroidSize = Large | Medium | Small
  deriving ( Eq , Show )

data Asteroid = Asteroid
  { astPos :: Vector
  , astVel :: Vector
  , astSize :: AsteroidSize
  } deriving ( Show )

data Explosion = Explosion
  { expPos :: Vector
  , expScale :: Float
  , expTimeLeft :: Float
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
                 , asteroids   :: [Asteroid]
                 , explosions  :: [Explosion]
                 , rng         :: StdGen
                 } deriving(Show)

initialState :: StdGen -> GameState
initialState initialRng = 
  let (asts, newRng) = generateAsteroids 5 initialRng
  in GameState 
       { screen      = Menu 
       , elapsedTime = 0 
       , ship        = initialShip 
       , bullets     = []
       , asteroids   = asts
       , explosions  = []
       , rng         = newRng
       }

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
      newPos   = wrap (x + vx * dt, y + vy * dt)
      newLife  = bulletLifeTime b - dt
  in b { bulletPos = newPos, bulletLifeTime = newLife }

updateBullets :: [Bullet] -> Float -> [Bullet]
updateBullets bs dt =
  [ moveBullet b dt | b <- bs, bulletLifeTime b - dt > 0 ]

generateAsteroids :: Int -> StdGen -> ([Asteroid], StdGen)
generateAsteroids 0 gen = ([], gen)
generateAsteroids n gen =
  let (x, gen1) = randomR (-250.0, 250.0) gen
      (y, gen2) = randomR (-250.0, 250.0) gen1
      ast       = Asteroid { astPos = (x, y), astVel = (0, 0), astSize = Large }
      (rest, finalGen) = generateAsteroids (n - 1) gen2
  in (ast : rest, finalGen)

updateExplosions :: [Explosion] -> Float -> [Explosion]
updateExplosions exps dt =
  [ e { expTimeLeft = expTimeLeft e - dt } | e <- exps, expTimeLeft e - dt > 0 ]

asteroidRadius :: AsteroidSize -> Float
asteroidRadius Large  = 30
asteroidRadius Medium = 20
asteroidRadius Small  = 10

shipRadius, bulletRadius :: Float
shipRadius   = 10
bulletRadius = 2.5

-- kortste afstand met wraparound
wrapDelta :: Float -> Float
wrapDelta d
  | d >  screenRadius = d - 2 * screenRadius
  | d < -screenRadius = d + 2 * screenRadius
  | otherwise         = d

-- squared distance, niet sqrt
circlesOverlap :: (Vector, Float) -> (Vector, Float) -> Bool
circlesOverlap ((x1, y1), r1) ((x2, y2), r2) =
  let dx = wrapDelta (x2 - x1)
      dy = wrapDelta (y2 - y1)
  in dx * dx + dy * dy <= (r1 + r2) * (r1 + r2)

bulletHitsAsteroid :: Bullet -> Asteroid -> Bool
bulletHitsAsteroid b a =
  circlesOverlap (bulletPos b, bulletRadius) (astPos a, asteroidRadius (astSize a))

shipHitsAsteroid :: Ship -> Asteroid -> Bool
shipHitsAsteroid s a =
  circlesOverlap (shipPos s, shipRadius) (astPos a, asteroidRadius (astSize a))

moveAsteroid :: Float -> Asteroid -> Asteroid
moveAsteroid dt a =  
  let (x, y)   = astPos a
      (vx, vy) = astVel a
  in a { astPos = wrap (x + vx * dt, y + vy * dt) }

smaller :: AsteroidSize -> Maybe AsteroidSize
smaller Large  = Just Medium
smaller Medium = Just Small
smaller Small  = Nothing

--asteroid halveert, als asteroid kleinst is dan verdwijnt die
splitAsteroid :: StdGen -> Asteroid -> ([Asteroid], StdGen)
splitAsteroid g a = case smaller (astSize a) of
  Nothing -> ([], g)
  Just sz ->
    let (ang, g1) = randomR (0, 2 * pi) g
        (spd, g2) = randomR (30, 80) g1
        v@(vx, vy) = (cos ang * spd, sin ang * spd)
        mk vel = a { astSize = sz, astVel = vel }
    in ([mk v, mk (-vx, -vy)], g2)