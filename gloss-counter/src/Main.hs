module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do
            highscorefile <- readFile "src/highscores.txt"
            let highscorefileint = read highscorefile :: Int
            playIO (InWindow "Counter" (768, 512) (0, 0))       -- Or FullScreen
                black                                           -- Background color
                144                                             -- Frames per second
                initialState{highscore = highscorefileint}      -- Initial state (With the highscore from the highscore file)
                view                                            -- View function
                input                                           -- Event function
                step                                            -- Step function