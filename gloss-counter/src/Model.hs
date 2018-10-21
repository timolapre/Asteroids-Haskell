-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss

data State = Running | GameOver | Paused

data Object =     Player {x :: Float, y :: Float, size :: Float, colour :: Color}
                | Asteroid {x :: Float, y :: Float, size :: Float, colour :: Color, dir :: Float}
                | AlienShip {x :: Float, y :: Float, size :: Float, colour :: Color}
                | Bullet {x :: Float, y :: Float, size :: Float, colour :: Color}

data Direction = North | East | South | West -- Up and Down is used for buttons which gives some problems hence the choice for compass directions

--nO_SECS_BETWEEN_CYCLES :: Float
--nO_SECS_BETWEEN_CYCLES = 0.1

playerColor = makeColor 255 0 0 1
asteroidColor = makeColor 255 255 0 1

data GameState = GameState {state :: State, objects :: [Object], elapsedTime :: Float}

player = Player {x = 40, y = 20, size = 100, colour = playerColor}
asteroid = Asteroid {x = -40, y = -20, size = 80, colour = asteroidColor, dir = 45}      

initialState :: GameState
initialState = GameState {state = Running, objects = [player, asteroid], elapsedTime = 0}