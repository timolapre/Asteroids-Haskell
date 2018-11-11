-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Controller

-- View (in IO)
view :: GameState -> IO Picture
view = return . viewPure

-- ViewPure (not IO)
viewPure :: GameState -> Picture
viewPure gstate = case state gstate of
                    Menu -> showMenuState gstate
                    Running -> showRunState gstate
                    GameOver -> showGameOverState gstate
                    Paused -> showPausedState gstate

-- Show Menu State
showMenuState gstate = pictures (concat (map  (toPicture (elapsedTime gstate)) (tail (objects gstate))))

-- Show Run State
showRunState gstate = pictures (concat (map  (toPicture (elapsedTime gstate)) (objects gstate)))

-- Show Game Over State
showGameOverState gstate = pictures (concat((toPicture (elapsedTime gstate) (getScore scoreText gstate)) : (toPicture (elapsedTime gstate) (getScore highscoreText gstate)) : (map  (toPicture (elapsedTime gstate)) gameoverState)))

-- Show Paused State
showPausedState gstate = pictures (concat (map  (toPicture (elapsedTime gstate)) (objects gstate)) ++ concat (map  (toPicture (elapsedTime gstate)) pausedState))

-- GameObject to on screen graphics (toPicture)
toPicture :: Float -> GameObject -> [Picture]
toPicture time object = case object of
                Player x y size dir speed boosting -> do 
                                                        let f = translate x y . Graphics.Gloss.rotate dir
                                                        let p = [Color playerColor $ f $ Polygon [(size/1.5,-size/1.5),(-size/1.5,-size/1.5),(0,size)]]
                                                        if boosting && mod (round (time*20)) 2 == 0 then p ++ [Color boostColor $ f $ Polygon [(size/3,-size/1.5),(0,-size),(-size/3,-size/1.5)]] else p
                Asteroid x y size dir -> [Color asteroidColor $ translate x y $ ThickCircle size 3]
                AlienShip x y size dir timer -> [Color alienColor $ translate x y $ Polygon [(size/2,size/2),(size/2,-size/2),(-size/2,-size/2),(-size/2,size/2)]]
                Bullet x y size dir -> [Color bulletColor $ translate x y $ circle size]
                AlienBullet x y size dir -> [Color bulletColor $ translate x y $ circle size]
                Tekst id intid x y string size colour -> [color colour $ translate x y $ Scale size size $ Text string]
