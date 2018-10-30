-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss

data State = Menu | Running | GameOver | Paused

data Vec2 = Vec2 (Float, Float)

data Object =     Player {x :: Float, y :: Float, size :: Float, dir :: Float, speed :: Vec2}
                | Asteroid {x :: Float, y :: Float, size :: Float, dir :: Float}
                | AlienShip {x :: Float, y :: Float, size :: Float, dir :: Float}
                | Bullet {x :: Float, y :: Float, size :: Float, dir :: Float}
                | Tekst {x :: Float, y :: Float, string :: String}

data Direction = North | East | South | West -- Up and Down is used for buttons which gives some problems hence the choice for compass directions
data TurnDir = Left | Right

data Input = Input {left :: Bool, right :: Bool, forward :: Bool, backward :: Bool}

playerColor = makeColor 255 0 0 1
asteroidColor = makeColor 255 255 0 1
alienColor = makeColor 255 0 255 1
bulletColor = makeColor 0 255 0 1
textColor = makeColor 255 255 255 1

data GameState = GameState {state :: State, objects :: [Object], elapsedTime :: Float, cntrls :: Input}

<<<<<<< HEAD
player = Player {x = 0, y = 0, size = 40, dir = 0, speed = Vec2(0,0)}
asteroid = Asteroid {x = -40, y = -20, size = 80, dir = 0}
alien = AlienShip {x = -40, y = -20, size = 80, dir = 0}

scoreText = Tekst {x = 200, y = 200, string = "0"}
=======
player = Player {x = 0, y = 0, size = 40, colour = playerColor, dir = 0, speed = Vec2(0,0)}
asteroid = Asteroid {x = -40, y = -20, size = 80, colour = asteroidColor, dir = 0}
>>>>>>> 40dffaf822c7dcabce81c7399d1fa6f5f400db0b

menuText = Tekst {x = -300, y = 0, string = "Menu, press Space to start"}
menuState :: [Object]
menuState = [menuText]

pausedText = Tekst {x = -300, y = 0, string = "Paused, press P to continue"}
pausedState :: [Object]
pausedState = [pausedText]

initialState :: GameState
initialState = GameState {state = Menu, objects = [player, alien], elapsedTime = 0, cntrls = Input{left = False, right = False, forward = False, backward = False}}
