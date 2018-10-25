-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss

data State = Menu | Running | GameOver | Paused

data Object =     Player {x :: Float, y :: Float, size :: Float, colour :: Color}
                | Asteroid {x :: Float, y :: Float, size :: Float, colour :: Color, dir :: Float}
                | AlienShip {x :: Float, y :: Float, size :: Float, colour :: Color}
                | Bullet {x :: Float, y :: Float, size :: Float, colour :: Color}

data Direction = North | East | South | West -- Up and Down is used for buttons which gives some problems hence the choice for compass directions

playerColor = makeColor 255 0 0 1
asteroidColor = makeColor 255 255 0 1

data GameState = GameState {state :: State, objects :: [Object], elapsedTime :: Float}

player = Player {x = 0, y = 0, size = 40, colour = playerColor}
asteroid = Asteroid {x = -40, y = -20, size = 80, colour = asteroidColor, dir = 0}      

initialState :: GameState
initialState = GameState {state = Running, objects = [player], elapsedTime = 0}