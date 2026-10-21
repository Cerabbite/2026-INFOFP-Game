-- 0 points up in gloss rotate, so 0 is pointing up
module Constants where

windowWidth, windowHeight :: Int
windowWidth  = 1000
windowHeight = 1000

halfWidth, halfHeight :: Float
halfWidth  = fromIntegral windowWidth  / 2
halfHeight = fromIntegral windowHeight / 2

fps :: Int
fps = 120

turnSpeed :: Float     -- degrees per second
turnSpeed = 360

thrustPower :: Float   -- acceleration per second squared
thrustPower = 250

dragFactor :: Float    -- fraction of speed kept after one second
dragFactor = 0.75