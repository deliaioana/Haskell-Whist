-- Main.hs
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import qualified Db
import User (CreateUserRequest (..))
import Web.Scotty
import qualified Data.Text.Lazy as L
import Lib 

--import Web.Scotty.Trans (ActionT, Options, ScottyT, defaultHandler, delete, get, json, jsonData, middleware, notFound, param, post, put, scottyOptsT, settings, showError, status, verbose)

main :: IO ()
main = do

  scotty 8081 $ do
    
    get "/game/init/:nrOfPlayers" $
      do
        numberOfPlayers <- param ("nrOfPlayers" :: L.Text) :: ActionM L.Text
        --numberOfPlayers <- param "nrOfPlayers"
        let game = initGame (parseDigitFromString numberOfPlayers) "username"
        json game

    post "/game/getNumberOfRounds" $
      do
        gameFromParam <- jsonData :: ActionM Lib.Game
        let numberOfRounds = getNumberOfRounds gameFromParam
        json numberOfRounds
    post "/game/initGameForRound/:round" $
      do
        gameFromParam <- jsonData :: ActionM Lib.Game
        round <- param ("round" :: L.Text) :: ActionM String
        let gameForRound = initGameForRound gameFromParam (parseNumberFromString round)
        json gameForRound
    post "/game/deal" $
      do
        gameFromParam <- jsonData :: ActionM Lib.Game
        let gameWithDealtCards = deal gameFromParam
        json gameWithDealtCards
    post "/game/currentPlayerMakeMove" $
      do
        gameFromParam <- jsonData :: ActionM Lib.Game
        let gameAfterMove = currentPlayerMakeMove gameFromParam
        json gameAfterMove