module Player where

import Model
import Constants

applyTurn :: Float -> Ship -> Ship
applyTurn dt s = s { shipDir = shipDir s + delta }
  where
    delta = case turnDir s of
      TurnLeft  -> negate (turnSpeed * dt)   -- met de klok mee
      TurnRight -> turnSpeed * dt
      NoTurn    -> 0

applyThrust :: Float -> Ship -> Ship
applyThrust dt s
  | isThrust s = s { shipVel = (vx + ax * dt, vy + ay * dt) }
  | otherwise  = s
  where
    (vx, vy) = shipVel s
    rad      = shipDir s * pi / 180
    ax       = thrustPower * sin rad   -- 0 degrees = up, sin=x en cos=y
    ay       = thrustPower * cos rad

applyDrag :: Float -> Ship -> Ship
applyDrag dt s = s { shipVel = (vx * k, vy * k) }
  where
    (vx, vy) = shipVel s
    k        = dragFactor ** dt        -- slowdown is onafhankelijk van framerate

moveShip :: Float -> Ship -> Ship
moveShip dt s = s { shipPos = wrap (x + vx * dt, y + vy * dt) }
  where
    (x, y)   = shipPos s
    (vx, vy) = shipVel s

updateShip :: Float -> Ship -> Ship
updateShip dt = moveShip dt . applyDrag dt . applyThrust dt . applyTurn dt

wrap :: Vector -> Vector
wrap (x, y) = (wrap1 halfWidth x, wrap1 halfHeight y)

wrap1 :: Float -> Float -> Float
wrap1 limit v
  | v >  limit = v - 2 * limit
  | v < -limit = v + 2 * limit
  | otherwise  = v