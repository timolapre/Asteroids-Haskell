{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game
import Data.Aeson
import qualified Data.ByteString.Lazy as B

main :: IO ()
main = do
            highscorefile <- B.readFile "src/highscores.json"
            let highscorefileint = case decode highscorefile of Just (HighscoreEntry _ value) -> value
                                                                _ -> 0
            playIO (InWindow "Counter" (768, 512) (0, 0))       -- Or FullScreen
                black                                           -- Background color
                144                                             -- Frames per second
                initialState{highscore = highscorefileint}      -- Initial state (With the highscore from the highscore file)
                view                                            -- View function
                input                                           -- Event function
                step                                            -- Step function