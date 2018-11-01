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

showMenuState gstate = pictures (concat (map  (toPicture (elapsedTime gstate)) menuState))

showRunState gstate = case objects gstate of
                        x -> pictures (concat (map  (toPicture (elapsedTime gstate)) x))

showGameOverState gstate = pictures (concat((toPicture (elapsedTime gstate) (getScore scoreText gstate)) : (toPicture (elapsedTime gstate) (getScore highscoreText gstate)) : (map  (toPicture (elapsedTime gstate)) gameoverState)))

showPausedState gstate = pictures (concat (map  (toPicture (elapsedTime gstate)) (objects gstate)) ++ concat (map  (toPicture (elapsedTime gstate)) pausedState))

toPicture :: Float -> Object -> [Picture]
toPicture time object = case object of
                Player x y size dir speed boosting -> do 
                                                        let f = translate x y . rotate dir
                                                        let p = [Color playerColor $ f $ Polygon [(size/1.5,-size/1.5),(-size/1.5,-size/1.5),(0,size)]]
                                                        if boosting && mod (round (time*20)) 2 == 0 then p ++ [Color boostColor $ f $ Polygon [(size/3,-size/1.5),(0,-size),(-size/3,-size/1.5)]] else p
                Asteroid x y size dir -> [Color asteroidColor $ translate x y $ ThickCircle size 3]
                AlienShip x y size dir -> [Color alienColor $ translate x y $ Polygon [(size/2,size/2),(size/2,-size/2),(-size/2,-size/2),(-size/2,size/2)]]
                Bullet x y size dir -> [Color bulletColor $ translate x y $ circle size]
                Tekst id x y string size -> [color textColor $ translate x y $ Scale size size $ Text string]

getScore :: Object -> GameState -> Object
getScore obj@Tekst{myID=id, x=_, y=_, string=_} gstate = case id of
                                                        "Highscore" -> obj{string = "Highscore: " ++ show (highscore gstate)}
                                                        "Score" -> obj{string = "Score: " ++ show (score gstate)}
                                                        "" -> obj
                                                        _ -> obj{string = "ERROR: ID not found"}
