{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveGeneric #-}


module Lib where
import Foreign.C.Types
import Foreign.C.String
import qualified Data.Text.Lazy as L
import Data.IORef (modifyIORef, newIORef, readIORef)

import Data.Aeson
import GHC.Generics

import Data.Text.Lazy             as TL
import Data.Text.Lazy.Encoding    as TL
import Data.Text.Lazy.IO          as TL

playGame :: Game -> Game
playGame game = playGameRounds game 1

playGameRounds :: Game -> Int -> Game
playGameRounds game round | round ==((getNumberOfRounds game)+1) = game
playGameRounds game round = playGameRounds (playRound game round) (round+1)  

getNumberOfRounds :: Game -> Int
getNumberOfRounds (G number _ _ _ _ _ _ _ _) = (number*3 + 12)

playRound :: Game -> Int -> Game
playRound game round = do 
                        let newGame = initGameForRound game round
                        let newGame2 = deal newGame
                        let newGame3 = computeBets newGame2
                        --let game = startHand game 
                        newGame

computeBets :: Game -> Game
computeBets (G nr pl deck trump nextPl score round hands x) = startBets (G nr pl deck trump nextPl score round hands x) nr

startBets :: Game -> Int -> Game
startBets game 0 = game --de tratat deparat game 1 ca e ultima licitatie
startBets (G nr pl deck trump 1 score round hands x) count = do
                                                                    let bet = "1"
                                                                    let intBet = parseBet bet hands
                                                                    let game = (G nr pl deck trump 2 score round hands x)
                                                                    let game2 = setPlayerBet game 1 intBet
                                                                    (startBets game2 (count-1))
startBets (G nr pl deck trump nextPl score round hands x) count = do
                                                                    let bet = parseDigit '1'
                                                                    --bet <- getComputerBet (G nr pl deck trump nextPl score round hands x)
                                                                    let followingPlayer = computeNextPlayer nextPl nr
                                                                    let game = (G nr pl deck trump followingPlayer score round hands x)
                                                                    let game2 = setPlayerBet game nextPl bet
                                                                    (startBets game2 (count-1))

getComputerBet :: Game -> Int
getComputerBet g = 1 -- de implementat cat liciteaza computerul

setPlayerBet :: Game -> Int -> Int -> Game
setPlayerBet (G nr pl deck trump nextPl score round hands x) player bet = (G nr pl deck trump nextPl (setPLayerBetInScoreboard score round player bet) round hands x)

setPLayerBetInScoreboard :: Scoreboard -> Int -> Int -> Int -> Scoreboard
setPLayerBetInScoreboard (S3 p1 s1 p2 s2 p3 s3 list) round pl bet = (S3 p1 s1 p2 s2 p3 s3 (setPlayerBetInList3 list round pl bet))
setPLayerBetInScoreboard (S4 p1 s1 p2 s2 p3 s3 p4 s4 list) round pl bet = (S4 p1 s1 p2 s2 p3 s3 p4 s4 (setPlayerBetInList4 list round pl bet))
setPLayerBetInScoreboard (S5 p1 s1 p2 s2 p3 s3 p4 s4 p5 s5 list) round pl bet = (S5 p1 s1 p2 s2 p3 s3 p4 s4 p5 s5 (setPlayerBetInList5 list round pl bet))
setPLayerBetInScoreboard (S6 p1 s1 p2 s2 p3 s3 p4 s4 p5 s5 p6 s6 list) round pl bet = (S6 p1 s1 p2 s2 p3 s3 p4 s4 p5 s5 p6 s6 (setPlayerBetInList6 list round pl bet))

setPlayerBetInList3 :: [ScoreLine3] -> Int -> Int -> Int -> [ScoreLine3]
setPlayerBetInList3 [] _ _ _ = []
setPlayerBetInList3 (x:xs) 1 p b = [setPlayerBetInLine3 x p b] 
setPlayerBetInList3 (x:xs) r p b = [x] ++ (setPlayerBetInList3 xs (r-1) p b)

setPlayerBetInLine3 :: ScoreLine3 -> Int -> Int -> ScoreLine3
setPlayerBetInLine3 (SL3 p1 p2 p3 p4 p5 p6 p7) 1 b = (SL3 p1 b p3 p4 p5 p6 p7)
setPlayerBetInLine3 (SL3 p1 p2 p3 p4 p5 p6 p7) 2 b = (SL3 p1 p2 p3 b p5 p6 p7)
setPlayerBetInLine3 (SL3 p1 p2 p3 p4 p5 p6 p7) 3 b = (SL3 p1 p2 p3 p4 p5 b p7)

setPlayerBetInList4 :: [ScoreLine4] -> Int -> Int -> Int -> [ScoreLine4]
setPlayerBetInList4 [] _ _ _ = []
setPlayerBetInList4 (x:xs) 1 p b = [setPlayerBetInLine4 x p b] 
setPlayerBetInList4 (x:xs) r p b = [x] ++ (setPlayerBetInList4 xs (r-1) p b)

setPlayerBetInLine4 :: ScoreLine4 -> Int -> Int -> ScoreLine4
setPlayerBetInLine4 (SL4 p1 p2 p3 p4 p5 p6 p7 p8 p9) 1 b = (SL4 p1 b p3 p4 p5 p6 p7 p8 p9)
setPlayerBetInLine4 (SL4 p1 p2 p3 p4 p5 p6 p7 p8 p9) 2 b = (SL4 p1 p2 p3 b p5 p6 p7 p8 p9)
setPlayerBetInLine4 (SL4 p1 p2 p3 p4 p5 p6 p7 p8 p9) 3 b = (SL4 p1 p2 p3 p4 p5 b p7 p8 p9)
setPlayerBetInLine4 (SL4 p1 p2 p3 p4 p5 p6 p7 p8 p9) 4 b = (SL4 p1 p2 p3 p4 p5 p6 p7 b p9)

setPlayerBetInList5 :: [ScoreLine5] -> Int -> Int -> Int -> [ScoreLine5]
setPlayerBetInList5 [] _ _ _ = []
setPlayerBetInList5 (x:xs) 1 p b = [setPlayerBetInLine5 x p b] 
setPlayerBetInList5 (x:xs) r p b = [x] ++ (setPlayerBetInList5 xs (r-1) p b)

setPlayerBetInLine5 :: ScoreLine5 -> Int -> Int -> ScoreLine5
setPlayerBetInLine5 (SL5 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11) 1 b = (SL5 p1 b p3 p4 p5 p6 p7 p8 p9 p10 p11)
setPlayerBetInLine5 (SL5 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11) 2 b = (SL5 p1 p2 p3 b p5 p6 p7 p8 p9 p10 p11)
setPlayerBetInLine5 (SL5 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11) 3 b = (SL5 p1 p2 p3 p4 p5 b p7 p8 p9 p10 p11)
setPlayerBetInLine5 (SL5 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11) 4 b = (SL5 p1 p2 p3 p4 p5 p6 p7 b p9 p10 p11)
setPlayerBetInLine5 (SL5 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11) 5 b = (SL5 p1 p2 p3 p4 p5 p6 p7 p8 p9 b p11)

setPlayerBetInList6 :: [ScoreLine6] -> Int -> Int -> Int -> [ScoreLine6]
setPlayerBetInList6 [] _ _ _ = []
setPlayerBetInList6 (x:xs) 1 p b = [setPlayerBetInLine6 x p b] 
setPlayerBetInList6 (x:xs) r p b = [x] ++ setPlayerBetInList6 xs (r-1) p b

setPlayerBetInLine6 :: ScoreLine6 -> Int -> Int -> ScoreLine6
setPlayerBetInLine6 (SL6 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13) 1 b = (SL6 p1 b p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13)
setPlayerBetInLine6 (SL6 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13) 2 b = (SL6 p1 p2 p3 b p5 p6 p7 p8 p9 p10 p11 p12 p13)
setPlayerBetInLine6 (SL6 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13) 3 b = (SL6 p1 p2 p3 p4 p5 b p7 p8 p9 p10 p11 p12 p13)
setPlayerBetInLine6 (SL6 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13) 4 b = (SL6 p1 p2 p3 p4 p5 p6 p7 b p9 p10 p11 p12 p13)
setPlayerBetInLine6 (SL6 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13) 5 b = (SL6 p1 p2 p3 p4 p5 p6 p7 p8 p9 b p11 p12 p13)
setPlayerBetInLine6 (SL6 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13) 6 b = (SL6 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 b p13)

deal :: Game -> Game
deal game = dealTrump (dealX (prepareGameForDeal game) (getHands game))

dealTrump :: Game -> Game
dealTrump (G number players deck trump nextPlayer scoreboard currentRound hands x) | Prelude.length deck > 0 = extractAndSetCardAsTrump (G number players deck trump nextPlayer scoreboard currentRound hands x) (getRandomCardFromDeck deck)
dealTrump x = x

extractAndSetCardAsTrump :: Game -> Card -> Game
extractAndSetCardAsTrump (G number players deck trump nextPlayer scoreboard currentRound hands x) randomCard = do
                                                                                                                  let newDeck = (extractCardFromDeck randomCard deck)
                                                                                                                  let newGame = (G number players newDeck randomCard nextPlayer scoreboard currentRound hands x)
                                                                                                                  (newGame)

prepareGameForDeal :: Game -> Game
prepareGameForDeal (G number players deck trump nextPlayer scoreboard currentRound hands x) = (G number (initPlayers number) (initCards number) (Val 0 '0') nextPlayer scoreboard currentRound hands x)

getHands :: Game -> Int
getHands (G _ _ _ _ _ _ _ h _) = h

setHands :: Game -> Int -> Game
setHands (G p1 p2 p3 p4 p5 p6 p7 p8 p9) x = (G p1 p2 p3 p4 p5 p6 p7 x p9) 

dealX :: Game -> Int -> Game
dealX (G a b c d e f g hands h) 0 = (G a b c d e f g hands h)
dealX (G number players c d e f g hands h) haveToDeal = dealX (dealOneCard (G number players c d e f g hands h) number) (haveToDeal-1)

dealOneCard :: Game -> Int -> Game
dealOneCard game 0 = game
dealOneCard (G number players deck trump nextPlayer scoreboard currentRound hands x) current = do
                                                                                                 let card = getRandomCardFromDeck deck
                                                                                                 let newDeck = (extractCardFromDeck card deck)
                                                                                                 let newPlayers = (addCardToXPlayer current card players)
                                                                                                 let newGame = (G number newPlayers newDeck trump nextPlayer scoreboard currentRound hands x)
                                                                                                 (dealOneCard newGame (current-1))

getCardAsString :: Card -> String
getCardAsString (Val val sign) = "Card: " ++ (show val) ++ [sign]

addCardToXPlayer :: Int -> Card -> Players -> Players
addCardToXPlayer 1 card (P3 p1 p2 p3) = (P3 (p1 ++ [card]) p2 p3)
addCardToXPlayer 2 card (P3 p1 p2 p3) = (P3 p1 (p2 ++ [card]) p3)
addCardToXPlayer 3 card (P3 p1 p2 p3) = (P3 p1 p2 (p3 ++ [card]))

addCardToXPlayer 1 card (P4 p1 p2 p3 p4) = (P4 (p1 ++ [card]) p2 p3 p4)
addCardToXPlayer 2 card (P4 p1 p2 p3 p4) = (P4 p1 (p2 ++ [card]) p3 p4)
addCardToXPlayer 3 card (P4 p1 p2 p3 p4) = (P4 p1 p2 (p3 ++ [card]) p4)
addCardToXPlayer 4 card (P4 p1 p2 p3 p4) = (P4 p1 p2 p3 (p4 ++ [card]))

addCardToXPlayer 1 card (P5 p1 p2 p3 p4 p5) = (P5 (p1 ++ [card]) p2 p3 p4 p5)
addCardToXPlayer 2 card (P5 p1 p2 p3 p4 p5) = (P5 p1 (p2 ++ [card]) p3 p4 p5)
addCardToXPlayer 3 card (P5 p1 p2 p3 p4 p5) = (P5 p1 p2 (p3 ++ [card]) p4 p5)
addCardToXPlayer 4 card (P5 p1 p2 p3 p4 p5) = (P5 p1 p2 p3 (p4 ++ [card]) p5)
addCardToXPlayer 5 card (P5 p1 p2 p3 p4 p5) = (P5 p1 p2 p3 p4 (p5 ++ [card]))

addCardToXPlayer 1 card (P6 p1 p2 p3 p4 p5 p6) = (P6 (p1 ++ [card]) p2 p3 p4 p5 p6)
addCardToXPlayer 2 card (P6 p1 p2 p3 p4 p5 p6) = (P6 p1 (p2 ++ [card]) p3 p4 p5 p6)
addCardToXPlayer 3 card (P6 p1 p2 p3 p4 p5 p6) = (P6 p1 p2 (p3 ++ [card]) p4 p5 p6)
addCardToXPlayer 4 card (P6 p1 p2 p3 p4 p5 p6) = (P6 p1 p2 p3 (p4 ++ [card]) p5 p6)
addCardToXPlayer 5 card (P6 p1 p2 p3 p4 p5 p6) = (P6 p1 p2 p3 p4 (p5 ++ [card]) p6)
addCardToXPlayer 6 card (P6 p1 p2 p3 p4 p5 p6) = (P6 p1 p2 p3 p4 p5 (p6 ++ [card]))

getRandomCardFromDeck :: [Card] -> Card -- de implementat altfel, momentan ia primul card
getRandomCardFromDeck (x:xs) = x

extractCardFromDeck :: Card -> [Card] -> [Card]
extractCardFromDeck _ []                 = []
extractCardFromDeck x (y:ys) | x == y    = extractCardFromDeck x ys
                    | otherwise = y : extractCardFromDeck x ys

initPlayers :: Int -> Players
initPlayers 3 = (P3 [] [] [])
initPlayers 4 = (P4 [] [] [] [])
initPlayers 5 = (P5 [] [] [] [] [])
initPlayers 6 = (P6 [] [] [] [] [] [])

data Players = P3 [Card] [Card] [Card]
                 | P4 [Card] [Card] [Card] [Card]
                 | P5 [Card] [Card] [Card] [Card] [Card]
                 | P6 [Card] [Card] [Card] [Card] [Card] [Card]
                 deriving (Show, Eq, Generic)

instance FromJSON Players
instance ToJSON Players

data Card = Val Int Char deriving (Show, Eq, Generic)

instance FromJSON Card
instance ToJSON Card

initCards :: Int -> [Card]
initCards 3 = getAllCardsFromXToY 14 9
initCards 4 = (initCards 3) ++ (getAllCardsOfValue 8) ++ (getAllCardsOfValue 7)
initCards 5 = (initCards 4) ++ (getAllCardsOfValue 6) ++ (getAllCardsOfValue 5)
initCards 6 = (initCards 5) ++ (getAllCardsOfValue 4) ++ (getAllCardsOfValue 3)

getAllCardsFromXToY :: Int -> Int -> [Card]
getAllCardsFromXToY x y | x == y = (getAllCardsOfValue x)
getAllCardsFromXToY x y = (getAllCardsOfValue x) ++ (getAllCardsFromXToY (x-1) y)

getAllCardsOfValue :: Int -> [Card]
getAllCardsOfValue x = [Val x 'C', Val x 'D', Val x 'H', Val x 'S'] 

parseNumberOfPlayers :: String -> Maybe Int
parseNumberOfPlayers "" = Nothing
parseNumberOfPlayers "\n" = Nothing
parseNumberOfPlayers (x:xs) = if (x <= '6' && x >= '3') then (Just (parseDigit x)) else Nothing

parseBet :: String -> Int -> Int
parseBet "" _ = (-1)
parseBet "\n" _ = (-1)
parseBet (x:xs) maxBet= if ((parseDigit x) <= maxBet && (parseDigit x) >= 0) then (parseDigit x) else (-1)

parseDigit :: Char -> Int
parseDigit x | x == '0' = 0
parseDigit x | x == '1' = 1
parseDigit x | x == '2' = 2
parseDigit x | x == '3' = 3
parseDigit x | x == '4' = 4
parseDigit x | x == '5' = 5
parseDigit x | x == '6' = 6
parseDigit x | x == '7' = 7
parseDigit x | x == '8' = 8
parseDigit x | x == '9' = 9
parseDigit _ = 0

parseDigitFromString :: L.Text -> Int
parseDigitFromString x | x == "0" = 0
parseDigitFromString x | x == "1" = 1
parseDigitFromString x | x == "2" = 2
parseDigitFromString x | x == "3" = 3
parseDigitFromString x | x == "4" = 4
parseDigitFromString x | x == "5" = 5
parseDigitFromString x | x == "6" = 6
parseDigitFromString x | x == "7" = 7
parseDigitFromString x | x == "8" = 8
parseDigitFromString x | x == "9" = 9
parseDigitFromString _ = 0

parseDigitFromString2 :: String -> Int
parseDigitFromString2 x | x == "0" = 0
parseDigitFromString2 x | x == "1" = 1
parseDigitFromString2 x | x == "2" = 2
parseDigitFromString2 x | x == "3" = 3
parseDigitFromString2 x | x == "4" = 4
parseDigitFromString2 x | x == "5" = 5
parseDigitFromString2 x | x == "6" = 6
parseDigitFromString2 x | x == "7" = 7
parseDigitFromString2 x | x == "8" = 8
parseDigitFromString2 x | x == "9" = 9
parseDigitFromString2 _ = 0

wrongNumberOfPlayersMessage :: String
wrongNumberOfPlayersMessage = "The specified number of players is incorrect!\nPlease try again without any additional characters!\n\n"

goodNumberOfPlayersMessage :: String -> String
goodNumberOfPlayersMessage x = "Your game will have " ++ x ++ " players.\n\n"

printThis :: String -> IO() 
printThis x = Prelude.putStr x

getIntFromMaybeInt :: (Maybe Int) -> Int
getIntFromMaybeInt (Just x) = x
getIntFromMaybeInt Nothing = 0

collectUntil :: (Monad m) => m a -> (a -> Bool) -> m [a]
collectUntil act f = do
  x <- act
  if f x
    then return []
    else (x:) <$> collectUntil act f

initGameForRound :: Game -> Int -> Game
initGameForRound (G number players deck trump nextPlayer scoreboard currentRound hands x) round = 
 (G number (initPlayers number) (initCards number) (Val 0 '0') (computeNextPlayerForInitRound nextPlayer) scoreboard round (computeNumberOfHands round number) 0) 

computeNextPlayerForInitRound :: Int -> Int
computeNextPlayerForInitRound current | current == 0 = 1
computeNextPlayerForInitRound current = current

computeNextPlayer :: Int -> Int -> Int
computeNextPlayer current number | current == number = 1
computeNextPlayer current number = (current+1)

computeNumberOfHands :: Int -> Int -> Int
computeNumberOfHands round number | round > number && round <= (number+6) = (round-number+1)
computeNumberOfHands round number | round > (number+6) && round <= (2*number+6) = 8
computeNumberOfHands round number | round > (2*number+6) && round <= (2*number+12) = (8 - (round - (2*number+6)))
computeNumberOfHands _ _ = 1

getScoreboardAsStringFromGame :: Game -> String
getScoreboardAsStringFromGame (G _ _ _ _ _ s _ _ _) = getScoreboardAsString s

getScoreboardAsString :: Scoreboard -> String
getScoreboardAsString (S3 p1 s1 p2 s2 p3 s3 list) = "Round no. " ++ p1 ++ " " ++ p2 ++ " " ++ p3 ++ "\n" ++ (getTable3AsString list 1)
getScoreboardAsString (S4 p1 s1 p2 s2 p3 s3 p4 s4 list) = "Round no. " ++ p1 ++ " " ++ p2 ++ " " ++ p3 ++ " " ++ p4 ++ "\n" ++ (getTable4AsString list 1)
getScoreboardAsString (S5 p1 s1 p2 s2 p3 s3 p4 s4 p5 s5 list) = "Round no. " ++ p1 ++ " " ++ p2 ++ " " ++ p3 ++ " " ++ p4 ++ " " ++ p5 ++ "\n" ++ (getTable5AsString list 1)
getScoreboardAsString (S6 p1 s1 p2 s2 p3 s3 p4 s4 p5 s5 p6 s6 list) = "Round no. " ++ p1 ++ " " ++ p2 ++ " " ++ p3 ++ " " ++ p4 ++ " " ++ p5 ++ " " ++ p6 ++ "\n" ++ (getTable6AsString list 1)

getTable3AsString :: [ScoreLine3] -> Int -> String
getTable3AsString [] _ = ""
getTable3AsString (x:xs) round = (getLine3AsString x round) ++ (getTable3AsString xs (round+1))

getLine3AsString :: ScoreLine3 -> Int -> String
getLine3AsString (SL3 r b1 s1 b2 s2 b3 s3) round = (itoa round) ++ ".    " ++ (itoa b1) ++ " " ++ (itoa s1) ++ " " 
 ++ (itoa b2) ++ " " ++ (itoa s2) ++ " " ++ (itoa b3) ++ " " ++ (itoa s3) ++ "\n"

getTable4AsString :: [ScoreLine4] -> Int -> String
getTable4AsString [] _ = ""
getTable4AsString (x:xs) round = (getLine4AsString x round) ++ (getTable4AsString xs (round+1))

getLine4AsString :: ScoreLine4 -> Int -> String
getLine4AsString (SL4 r b1 s1 b2 s2 b3 s3 b4 s4) round = (itoa round) ++ ".    " ++ (itoa b1) ++ " " ++ (itoa s1) ++ " " 
 ++ (itoa b2) ++ " " ++ (itoa s2) ++ " " ++ (itoa b3) ++ " " ++ (itoa s3) ++ " " ++ (itoa b4) ++ " " ++ (itoa s4) ++ "\n"

getTable5AsString :: [ScoreLine5] -> Int -> String
getTable5AsString [] _ = ""
getTable5AsString (x:xs) round = (getLine5AsString x round) ++ (getTable5AsString xs (round+1))

getLine5AsString :: ScoreLine5 -> Int -> String
getLine5AsString (SL5 r b1 s1 b2 s2 b3 s3 b4 s4 b5 s5) round = (itoa round) ++ ".    " ++ (itoa b1) ++ " " ++ (itoa s1) ++ " " 
 ++ (itoa b2) ++ " " ++ (itoa s2) ++ " " ++ (itoa b3) ++ " " ++ (itoa s3) ++ " " ++ (itoa b4) ++ " " ++ (itoa s4) ++ " " 
 ++ (itoa b5) ++ " " ++ (itoa s5) ++ "\n"

getTable6AsString :: [ScoreLine6] -> Int -> String
getTable6AsString [] _ = ""
getTable6AsString (x:xs) round = (getLine6AsString x round) ++ (getTable6AsString xs (round+1))

getLine6AsString :: ScoreLine6 -> Int -> String
getLine6AsString (SL6 r b1 s1 b2 s2 b3 s3 b4 s4 b5 s5 b6 s6) round = (itoa round) ++ ".    " ++ (itoa b1) ++ " " ++ (itoa s1) ++ " " 
 ++ (itoa b2) ++ " " ++ (itoa s2) ++ " " ++ (itoa b3) ++ " " ++ (itoa s3) ++ " " ++ (itoa b4) ++ " " ++ (itoa s4) ++ " " 
 ++ (itoa b5) ++ " " ++ (itoa s5) ++ " " ++ (itoa b6) ++ " " ++ (itoa s6) ++ "\n"

itoa :: Int -> String 
itoa 0 = "0"
itoa 1 = "1"
itoa 2 = "2"
itoa 3 = "3"
itoa 4 = "4"
itoa 5 = "5"
itoa 6 = "6"
itoa 7 = "7"
itoa 8 = "8"
itoa 9 = "9"
itoa x = (itoa (x `div` 10)) ++ (itoa (x-(x `div` 10)))

data Game = G Int Players [Card] Card Int Scoreboard Int Int Int deriving (Show, Generic)
-- in order:  	Number of players, Players, Deck, Trump card, Player who is next by index, Scoreboard, 
-- 				Current round number, Current number of cards dealt, Current number of cards played

instance FromJSON Game
instance ToJSON Game

data Scoreboard = S3 String Int String Int String Int [ScoreLine3]
                | S4 String Int String Int String Int String Int [ScoreLine4]
                | S5 String Int String Int String Int String Int String Int [ScoreLine5]
                | S6 String Int String Int String Int String Int String Int String Int [ScoreLine6] 
                deriving (Show, Generic)

instance FromJSON Scoreboard
instance ToJSON Scoreboard

data ScoreLine3 = SL3 Int Int Int Int Int Int Int deriving (Show, Generic)
data ScoreLine4 = SL4 Int Int Int Int Int Int Int Int Int deriving (Show, Generic)
data ScoreLine5 = SL5 Int Int Int Int Int Int Int Int Int Int Int deriving (Show, Generic)
data ScoreLine6 = SL6 Int Int Int Int Int Int Int Int Int Int Int Int Int deriving (Show, Generic)

instance FromJSON ScoreLine3
instance ToJSON ScoreLine3
instance FromJSON ScoreLine4
instance ToJSON ScoreLine4
instance FromJSON ScoreLine5
instance ToJSON ScoreLine5
instance FromJSON ScoreLine6
instance ToJSON ScoreLine6

computeFinalWinner :: Game -> String
computeFinalWinner (G _ _ _ _ _ scoreboard _ _ _) = computeFinalWinnerFromScoreboard scoreboard

computeFinalWinnerFromScoreboard :: Scoreboard -> String
computeFinalWinnerFromScoreboard (S3 p1 x1 p2 x2 p3 x3 _) | x1>=x2 && x1>=x3 = p1 
computeFinalWinnerFromScoreboard (S3 p1 x1 p2 x2 p3 x3 _) | x2>=x1 && x2>=x3 = p2
computeFinalWinnerFromScoreboard (S3 p1 x1 p2 x2 p3 x3 _) | x3>=x2 && x3>=x1 = p2

computeFinalWinnerFromScoreboard (S4 p1 x1 p2 x2 p3 x3 p4 x4 _) | x1>=x2 && x1>=x3 && x1>=x4 = p1
computeFinalWinnerFromScoreboard (S4 p1 x1 p2 x2 p3 x3 p4 x4 _) | x2>=x3 && x2>=x1 && x2>=x4 = p2
computeFinalWinnerFromScoreboard (S4 p1 x1 p2 x2 p3 x3 p4 x4 _) | x3>=x2 && x3>=x1 && x3>=x4 = p3
computeFinalWinnerFromScoreboard (S4 p1 x1 p2 x2 p3 x3 p4 x4 _) | x4>=x2 && x4>=x3 && x4>=x1 = p4

computeFinalWinnerFromScoreboard (S5 p1 x1 p2 x2 p3 x3 p4 x4 p5 x5 _) | x1>=x2 && x1>=x3 && x1>=x4 && x1>=x5 = p1
computeFinalWinnerFromScoreboard (S5 p1 x1 p2 x2 p3 x3 p4 x4 p5 x5 _) | x2>=x1 && x2>=x3 && x2>=x4 && x2>=x5 = p2
computeFinalWinnerFromScoreboard (S5 p1 x1 p2 x2 p3 x3 p4 x4 p5 x5 _) | x3>=x2 && x3>=x1 && x3>=x4 && x3>=x5 = p3
computeFinalWinnerFromScoreboard (S5 p1 x1 p2 x2 p3 x3 p4 x4 p5 x5 _) | x4>=x2 && x4>=x3 && x4>=x1 && x4>=x5 = p4
computeFinalWinnerFromScoreboard (S5 p1 x1 p2 x2 p3 x3 p4 x4 p5 x5 _) | x5>=x2 && x5>=x3 && x5>=x4 && x5>=x1 = p5

computeFinalWinnerFromScoreboard (S6 p1 x1 p2 x2 p3 x3 p4 x4 p5 x5 p6 x6 _) | x1>=x2 && x1>=x3 && x1>=x4 && x1>=x5 && x1>=x6= p1
computeFinalWinnerFromScoreboard (S6 p1 x1 p2 x2 p3 x3 p4 x4 p5 x5 p6 x6 _) | x2>=x1 && x2>=x3 && x2>=x4 && x2>=x5 && x2>=x6= p2
computeFinalWinnerFromScoreboard (S6 p1 x1 p2 x2 p3 x3 p4 x4 p5 x5 p6 x6 _) | x3>=x2 && x3>=x1 && x3>=x4 && x3>=x5 && x3>=x6= p3
computeFinalWinnerFromScoreboard (S6 p1 x1 p2 x2 p3 x3 p4 x4 p5 x5 p6 x6 _) | x4>=x2 && x4>=x3 && x4>=x1 && x4>=x5 && x4>=x6= p4
computeFinalWinnerFromScoreboard (S6 p1 x1 p2 x2 p3 x3 p4 x4 p5 x5 p6 x6 _) | x5>=x2 && x5>=x3 && x5>=x4 && x5>=x1 && x5>=x6= p5
computeFinalWinnerFromScoreboard (S6 p1 x1 p2 x2 p3 x3 p4 x4 p5 x5 p6 x6 _) | x6>=x2 && x6>=x3 && x6>=x4 && x6>=x5 && x6>=x1= p6

initScores :: Int -> String -> Scoreboard
initScores 3 name = (S3 name 0 "P2" 0 "P3" 0 [])
initScores 4 name = (S4 name 0 "P2" 0 "P3" 0 "P4" 0 [])
initScores 5 name = (S5 name 0 "P2" 0 "P3" 0 "P4" 0 "P5" 0 [])
initScores 6 name = (S6 name 0 "P2" 0 "P3" 0 "P4" 0 "P5" 0 "P6" 0 [])

initGameString :: Int -> String -> L.Text
initGameString number playerName = TL.pack (show (G number (initPlayers number) (initCards number) (Val 0 '0') 0 (initScores number playerName) 0 0 0))

initGame :: Int -> String -> Game
initGame number playerName = (G number (initPlayers number) (initCards number) (Val 0 '0') 0 (initScores number playerName) 0 0 0)

dottedLine :: String
dottedLine = "------------------------------------------------------------\n"

returnGameRulesIfWanted :: String -> String
returnGameRulesIfWanted "y" = "rules"--rules
returnGameRulesIfWanted _ = "Ok.\n"

rules :: IO CString
rules = newCString ("okokokok")

parseNumberFromLText :: L.Text -> Int
parseNumberFromLText x = parseDigitFromString x

parseNumberFromString :: String -> Int
parseNumberFromString "" = 0
parseNumberFromString x | (Prelude.length (x)) == 1 = parseDigitFromString2 x
parseNumberFromString (x:xs) = (10 * (parseDigit x) + (parseNumberFromString xs))

currentPlayerMakeMove :: Game -> Game
currentPlayerMakeMove (G number players deck trump nextPlayer scoreboard currentRound hands x) = (G number (playerPlayCard players nextPlayer (firstPlayer currentRound number)) deck trump (computeNextPlayer nextPlayer number) scoreboard currentRound hands (x+1))

firstPlayer :: Int -> Int -> Int
firstPlayer x y = x `mod` y

playerPlayCard :: Players -> Int -> Int -> Players
playerPlayCard players x y = playAnyCard players x
--playerPlayCard players x y | x == y = playAnyCard players x
--playerPlayCard players x y = playAnyCorrectCard players x (getFirstPlayedCard players y) 
--arguments: players, next player, the person playing first this round

playAnyCard :: Players -> Int -> Players --just for bot players. player one decides for himself
playAnyCard (P3 p1 p2 p3) 2 = (P3 p1 (playAnyCardFromThisDeck p2) p3)
playAnyCard (P3 p1 p2 p3) 3 = (P3 p1 p2 (playAnyCardFromThisDeck p3))

playAnyCard (P4 p1 p2 p3 p4) 2 = (P4 p1 (playAnyCardFromThisDeck p2) p3 p4)
playAnyCard (P4 p1 p2 p3 p4) 3 = (P4 p1 p2 (playAnyCardFromThisDeck p3) p4)
playAnyCard (P4 p1 p2 p3 p4) 4 = (P4 p1 p2 p3 (playAnyCardFromThisDeck p4))

playAnyCard (P5 p1 p2 p3 p4 p5) 2 = (P5 p1 (playAnyCardFromThisDeck p2) p3 p4 p5)
playAnyCard (P5 p1 p2 p3 p4 p5) 3 = (P5 p1 p2 (playAnyCardFromThisDeck p3) p4 p5)
playAnyCard (P5 p1 p2 p3 p4 p5) 4 = (P5 p1 p2 p3 (playAnyCardFromThisDeck p4) p5)
playAnyCard (P5 p1 p2 p3 p4 p5) 5 = (P5 p1 p2 p3 p4 (playAnyCardFromThisDeck p5))

playAnyCard (P6 p1 p2 p3 p4 p5 p6) 2 = (P6 p1 (playAnyCardFromThisDeck p2) p3 p4 p5 p6)
playAnyCard (P6 p1 p2 p3 p4 p5 p6) 3 = (P6 p1 p2 (playAnyCardFromThisDeck p3) p4 p5 p6)
playAnyCard (P6 p1 p2 p3 p4 p5 p6) 4 = (P6 p1 p2 p3 (playAnyCardFromThisDeck p4) p5 p6)
playAnyCard (P6 p1 p2 p3 p4 p5 p6) 5 = (P6 p1 p2 p3 p4 (playAnyCardFromThisDeck p5) p6)
playAnyCard (P6 p1 p2 p3 p4 p5 p6) 6 = (P6 p1 p2 p3 p4 p5 (playAnyCardFromThisDeck p6))

playAnyCardFromThisDeck :: [Card] -> [Card]
playAnyCardFromThisDeck deck = do
                                 let card = (getRandomCardFromDeck deck)
                                 (putFirstThisCardInDeck card (extractCardFromDeck card deck))

putFirstThisCardInDeck :: Card  -> [Card] -> [Card]
putFirstThisCardInDeck card [] = [card]
putFirstThisCardInDeck card x = card : x

--arguments: players, next player, first played card
playAnyCorrectCard :: Players -> Int -> Card -> Players
playAnyCorrectCard (P3 p1 p2 p3) 2 card = (P3 p1 (playAnyCorrectCardFromThisDeck p2 card) p3)
--to be continued for all types of Players

-- this method is not done !!
playAnyCorrectCardFromThisDeck :: [Card] -> Card -> [Card]
playAnyCorrectCardFromThisDeck x card = x

getFirstPlayedCard :: Players -> Int -> Card
getFirstPlayedCard (P3 p1 p2 p3) 1 = getFirstCardFromList p1
getFirstPlayedCard (P3 p1 p2 p3) 2 = getFirstCardFromList p2
getFirstPlayedCard (P3 p1 p2 p3) 3 = getFirstCardFromList p3

getFirstPlayedCard (P4 p1 p2 p3 p4) 1 = getFirstCardFromList p1
getFirstPlayedCard (P4 p1 p2 p3 p4) 2 = getFirstCardFromList p2
getFirstPlayedCard (P4 p1 p2 p3 p4) 3 = getFirstCardFromList p3
getFirstPlayedCard (P4 p1 p2 p3 p4) 4 = getFirstCardFromList p4

getFirstPlayedCard (P5 p1 p2 p3 p4 p5) 1 = getFirstCardFromList p1
getFirstPlayedCard (P5 p1 p2 p3 p4 p5) 2 = getFirstCardFromList p2
getFirstPlayedCard (P5 p1 p2 p3 p4 p5) 3 = getFirstCardFromList p3
getFirstPlayedCard (P5 p1 p2 p3 p4 p5) 4 = getFirstCardFromList p4
getFirstPlayedCard (P5 p1 p2 p3 p4 p5) 5 = getFirstCardFromList p5

getFirstPlayedCard (P6 p1 p2 p3 p4 p5 p6) 1 = getFirstCardFromList p1
getFirstPlayedCard (P6 p1 p2 p3 p4 p5 p6) 2 = getFirstCardFromList p2
getFirstPlayedCard (P6 p1 p2 p3 p4 p5 p6) 3 = getFirstCardFromList p3
getFirstPlayedCard (P6 p1 p2 p3 p4 p5 p6) 4 = getFirstCardFromList p4
getFirstPlayedCard (P6 p1 p2 p3 p4 p5 p6) 5 = getFirstCardFromList p5
getFirstPlayedCard (P6 p1 p2 p3 p4 p5 p6) 6 = getFirstCardFromList p6

getFirstCardFromList :: [Card] -> Card
getFirstCardFromList [] = (Val 0 '0') --null card
getFirstCardFromList (x:xs) = x

foreign export ccall "rules" rules :: IO CString