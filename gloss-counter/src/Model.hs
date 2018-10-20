-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss

data State = Running | GameOver | Paused
data Objects = Player Info | Asteroid Info | AlienShip Info | Bullet Info
data Info = Info {x :: Float, y :: Float, size :: Float, colour :: Color}
data Direction = North | East | South | West

--nO_SECS_BETWEEN_CYCLES :: Float
--nO_SECS_BETWEEN_CYCLES = 0.1

playerColor = makeColor 255 0 0 1
asteroidColor = makeColor 255 255 0 1

data GameState = GameState {state :: State, objects :: [Objects], elapsedTime :: Float}

player = Player Info {x = 40, y = 20, size = 100, colour = playerColor}
asteroid = Asteroid Info {x = -40, y = -20, size = 80, colour = asteroidColor}             

initialState :: GameState
initialState = GameState {state = Running, objects = [player, asteroid], elapsedTime = 0}