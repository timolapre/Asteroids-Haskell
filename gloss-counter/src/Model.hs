-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss

-- States of the game
data State = Menu | Running | GameOver | Paused

-- Vector2
data Vec2 = Vec2 (Float, Float)

-- All possible Objects
data Object =     Player {x :: Float, y :: Float, size :: Float, dir :: Float, speed :: Vec2, boosting :: Bool}
                | Asteroid {x :: Float, y :: Float, size :: Float, dir :: Float}
                | AlienShip {x :: Float, y :: Float, size :: Float, dir :: Float}
                | Bullet {x :: Float, y :: Float, size :: Float, dir :: Float}
                | Tekst {myID :: String, x :: Float, y :: Float, string :: String, size :: Float}

-- Direction                
data Direction = North | East | South | West
--TurnDirection
data TurnDir = Left | Right
-- Player Input for movement
data Input = Input {left :: Bool, right :: Bool, forward :: Bool, backward :: Bool}

-- GameState
data GameState = GameState {state :: State, objects :: [Object], elapsedTime :: Float, cntrls :: Input, lives :: Int, score :: Int, highscore :: Int}

-- All colors
playerColor = makeColor 255 0 0 1
boostColor = makeColor 255 200 200 1
asteroidColor = makeColor 255 255 0 1
alienColor = makeColor 255 0 255 1
bulletColor = makeColor 0 255 0 1
textColor = makeColor 255 255 255 1

-- pre-made Objects (currently used or used in the past for testing purposes)
player = Player {x = 0, y = 0, size = 30, dir = 0, speed = Vec2(0,0), boosting = False}
asteroid = Asteroid {x = -40, y = -20, size = 80, dir = 0}
alien = AlienShip {x = -40, y = -20, size = 30, dir = 0}

-- pre-made Text Objects
livesText = Tekst {myID = "Lives", x = 230, y = 175, string = "", size = 0.3}
scoreText = Tekst {myID = "Score", x = 0, y = 175, string = "", size = 0.3}
highscoreText = Tekst {myID = "Highscore", x = 0, y = 125, string = "", size = 0.3}
pauseText = Tekst {myID = "", x = 200, y = 225, string = "Press p to Pause", size = 0.15}

-- menuState (= 1 or multiple Text objects)
menuText = Tekst {myID = "", x = -300, y = 0, string = "Menu, press Space to start", size = 0.3}
menuState :: [Object]
menuState = [menuText]

-- pausedState (= 1 or multiple Text objects)
pausedText = Tekst {myID = "", x = -300, y = 0, string = "Paused, press P to continue", size = 0.3}
pausedState :: [Object]
pausedState = [pausedText]

-- gameverState (= 1 or multiple Text objects)
gameoverText = Tekst {myID = "", x = -250, y = 0, string = "Game Over! press Space to play again", size = 0.2}
gameoverState :: [Object]
gameoverState = [gameoverText]

-- initialState
initialState :: GameState
initialState = GameState {state = Menu, objects = [player, alien, scoreText, pauseText, livesText], elapsedTime = 0, cntrls = Input{left = False, right = False, forward = False, backward = False}, lives = 3, score = 0}
