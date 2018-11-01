-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = case state gstate of
                    Menu -> showMenuState gstate
                    Running -> showRunState gstate
                    GameOver -> showGameOverState gstate
                    Paused -> showPausedState gstate

showMenuState gstate = pictures (map toPicture menuState)

showRunState gstate = case objects gstate of
                        x -> pictures (map toPicture x)

showGameOverState gstate = pictures (toPicture (getScore scoreText gstate) : toPicture (getScore highscoreText gstate) : (map toPicture gameoverState))

showPausedState gstate = pictures (map toPicture (objects gstate) ++ (map toPicture pausedState))

toPicture :: Object -> Picture
toPicture object = case object of
                Player x y size dir speed -> Color playerColor $ translate x y $ rotate dir $ Polygon [(size/1.5,-size/1.5),(-size/1.5,-size/1.5),(0,size)]
                Asteroid x y size dir -> Color asteroidColor $ translate x y $ ThickCircle size 3
                AlienShip x y size dir -> Color alienColor $ translate x y $ Polygon [(size/2,size/2),(size/2,-size/2),(-size/2,-size/2),(-size/2,size/2)]
                Bullet x y size dir -> Color bulletColor $ translate x y $ circle size
                Tekst id x y string size -> color textColor $ translate x y $ Scale size size $ Text string

getScore :: Object -> GameState -> Object
getScore obj@Tekst{myID=id, x=_, y=_, string=_} gstate = case id of
                                                        "Highscore" -> obj{string = "Highscore: " ++ show (highscore gstate)}
                                                        "Score" -> obj{string = "Score: " ++ show (score gstate)}
                                                        "" -> obj
                                                        _ -> obj{string = "ERROR: ID not found"}