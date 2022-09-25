package game;

import apiCallers.*;
import graphics.MainFrame;
import org.json.JSONArray;
import org.json.JSONObject;

import javax.swing.*;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;

public class Game {
    private int numberOfPlayers;
    private List<List<Card>> players = new ArrayList<>();
    private int numberOfRounds;
    private List<Card> deck;
    private Card trump;
    private int nextPlayer;
    private int currentRound;
    private int currentNumberOfCardsDealt;
    private int currentNumberOfCardsPlayed;
    private Scoreboard scoreboard;
    private List<Card> playedCards;
    private Card pressedCard;
    private boolean playerHasToPressCard = false;

    private InitGameApiCaller initGameApiCaller = new InitGameApiCaller();
    private GetNumberOfRoundsApiCaller getNumberOfRoundsApiCaller = new GetNumberOfRoundsApiCaller();
    private InitGameForRoundApiCaller initGameForRoundApiCaller = new InitGameForRoundApiCaller();
    private DealCardsForRoundApiCaller dealCardsForRoundApiCaller = new DealCardsForRoundApiCaller();
    private CurrentPlayerMakeMoveApiCaller currentPlayerMakeMoveApiCaller = new CurrentPlayerMakeMoveApiCaller();

    final MainFrame frame;

    public Game(MainFrame frame) {
        this.frame = frame;
    }

    public void initGame(int nrOfPlayers) {
        setNumberOfPlayers(nrOfPlayers);
        String jsonResp = initGameApiCaller.sendRequest(nrOfPlayers);
        print(jsonResp);
        mapGame(jsonResp);
        Integer numberOfRoundsFromJSON = 0;
        try {
            String numberOfRoundsJSON = getNumberOfRoundsApiCaller.sendRequest(getGameAsJSON());
            numberOfRoundsFromJSON = Integer.valueOf(numberOfRoundsJSON);
            setNumberOfRounds(numberOfRoundsFromJSON);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        setPressedCard(new Card(0, '0')); //null card
    }

    private String getGameAsJSON() {
        JSONArray gameJSON = new JSONArray();
        gameJSON.put(numberOfPlayers);

        JSONObject playersObject = new JSONObject();
        JSONArray playerArray = new JSONArray();
        for(int i = 0; i < numberOfPlayers; ++i) {
            JSONArray iPlayerCards = new JSONArray();
            for (Card card : players.get(i)) {
                iPlayerCards.put(card.toJSON());
            }
            playerArray.put(iPlayerCards);
        }

        playersObject.put("contents", playerArray);
        playersObject.put("tag", "P"+numberOfPlayers);
        gameJSON.put(playersObject);

        //append deck
        JSONArray deckJSON = new JSONArray();
        for (Card card : deck) {
            deckJSON.put(card.toJSON());
        }
        gameJSON.put(deckJSON);

        //append trump
        gameJSON.put(trump.toJSON());

        //append next player
        gameJSON.put(nextPlayer);

        //append scoreboard
        gameJSON.put(scoreboard.toJSON());

        //append rest of values
        gameJSON.put(currentRound);
        gameJSON.put(currentNumberOfCardsDealt);
        gameJSON.put(currentNumberOfCardsPlayed);

        return gameJSON.toString();
    }

    public void mapGame(String game) {
        JSONArray jsonArray = new JSONArray(game);

        //mapping player cards
        initPlayers(numberOfPlayers);
        JSONObject playerCardsObject = jsonArray.getJSONObject(1);

        String line = playerCardsObject.get("contents").toString();
        line = line.substring(1, line.length()-1);
        line = line.replace("]],", "]]&");
        line = line.replace("[],", "[]&");

        String[] words = line.split("&");

        for(int i = 0; i < numberOfPlayers; ++i) {
            if(!words[i].equals("[]")) {
                String word = words[i].substring(2, words[i].length()-2);
                word = word.replace("],[", "&");
                String[] cards = word.split("&");
                for (String strCard : cards) {
                    Card card;
                    card = parseCard(strCard);
                    players.get(i).add(card);
                }
            }
        }

        //mapping the remaining deck
        setDeck(new ArrayList<>());
        String deckLine;

        deckLine = jsonArray.get(2).toString();
        deckLine = deckLine.substring(1, deckLine.length()-1);
        if(!deckLine.isEmpty()) {
            deckLine = deckLine.substring(1, deckLine.length()-1);
            //noinspection DuplicatedCode
            deckLine = deckLine.replace("],[", "&");

            String[] deckCards = deckLine.split("&");
            for (String strCard : deckCards) {
                Card card;
                card = parseCard(strCard);
                deck.add(card);
            }
        }

        //mapping the trump card
        String trumpStr = jsonArray.get(3).toString();
        trumpStr = trumpStr.substring(1, trumpStr.length()-1);
        Card trumpCard = parseCard(trumpStr);
        setTrump(trumpCard);

        //mapping simple numbers
        setNextPlayer(jsonArray.getInt(4));
        setCurrentRound(jsonArray.getInt(6));
        setCurrentNumberOfCardsDealt(jsonArray.getInt(7));
        setCurrentNumberOfCardsPlayed(jsonArray.getInt(8));

        //mapping the scoreboard
        JSONObject scoreboardObject = jsonArray.getJSONObject(5);
        String scoreboardString = scoreboardObject.get("contents").toString();
        scoreboardString = scoreboardString.substring(2, scoreboardString.length()-1);
        scoreboardString = scoreboardString.replace("\",","&");
        scoreboardString = scoreboardString.replace(",\"","&");
        scoreboardString = scoreboardString.replace(",[","&[");
        String[] scoreboardParts = scoreboardString.split("&");

        String playerName = scoreboardParts[0];
        initScoreboard(numberOfPlayers, playerName);
        List<Integer> playerPoints = new ArrayList<>();
        for(int i = 0; i < numberOfPlayers; ++i) {
            playerPoints.add(Integer.valueOf(scoreboardParts[i*2+1]));
        }
        scoreboard.setPlayerPoints(playerPoints);
    }

    private void initScoreboard(int numberOfPlayers, String playerName) {
        setScoreboard(new Scoreboard(numberOfPlayers, playerName));
    }

    private void initPlayers(int numberOfPlayers) {
        this.players = new ArrayList<>();
        for(int i=0; i<numberOfPlayers; ++i) {
            this.players.add(new ArrayList<>());
        }
    }

    private Card parseCard(String strCard) {
        Card card = new Card();
        int value;
        char sign;

        if(strCard.charAt(1) == ',') {
            value = strCard.charAt(0) - 48;
            sign = strCard.charAt(3);
        }
        else {
            value = (strCard.charAt(0) - 48) * 10 + (strCard.charAt(1) - 48);
            sign = strCard.charAt(4);
        }

        card.setValue(value);
        card.setSign(sign);
        return card;
    }

    public void print(String x) {
        System.out.println(x);
    }

    private void setNumberOfPlayers(int nrOfPlayers) {
        numberOfPlayers = nrOfPlayers;
    }

    public void playGame(int nrOfPlayers) {
        initGame(nrOfPlayers);
        setCurrentRound(1);
        while(currentRound <= numberOfRounds) {
            playRound(currentRound);
            setCurrentRound(currentRound+1);
        }
    }

    private void playRound(int currentRound) {
        prepareCardsForRound(currentRound);
        dealCardsForCurrentRound();
        if(nextPlayer == 1) {
            //wait until player chooses card
            setPlayerHasToPressCard(true);

            ThreadCounter threadCounter = new ThreadCounter();
            threadCounter.start();

            while(isPlayerHasToPressCard()) {
                try {
                    TimeUnit.SECONDS.sleep(1);
                } catch (InterruptedException e) {
                    throw new RuntimeException(e);
                }
            }
        }
        else {
            try {
                TimeUnit.SECONDS.sleep(3);
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
            currentPlayerMakeMove();
        }
    }

    private void currentPlayerMakeMove() {
        String newGame = "";
        try {
            newGame = currentPlayerMakeMoveApiCaller.sendRequest(getGameAsJSON());
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        mapGame(newGame);
        refreshVisuals();
    }

    private void dealCardsForCurrentRound() {
        String gameResponse = "";
        try {
            gameResponse = dealCardsForRoundApiCaller.sendRequest(getGameAsJSON());
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        print("Game after deal: \n" + gameResponse);
        mapGame(gameResponse);
        refreshVisuals();
    }

    private void prepareCardsForRound(int currentRound) {
        String gameResponse = "";
        try {
            gameResponse = initGameForRoundApiCaller.sendRequest(currentRound, getGameAsJSON());
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        print("Game after preparing: \n" + gameResponse);
        mapGame(gameResponse);
        refreshVisuals();
    }

    private void refreshVisuals() {
        print("\nRefreshing\n");
        //SwingUtilities.updateComponentTreeUI(frame);
        //frame.getCanvas().refreshCanvas();
        frame.refreshCanvas();
    }

    public int getNumberOfPlayers() {
        return numberOfPlayers;
    }

    public List<List<Card>> getPlayers() {
        return players;
    }

    public void setPlayers(List<List<Card>> players) {
        this.players = players;
    }

    public int getNumberOfRounds() {
        return numberOfRounds;
    }

    public void setNumberOfRounds(int numberOfRounds) {
        this.numberOfRounds = numberOfRounds;
    }

    public List<Card> getDeck() {
        return deck;
    }

    public void setDeck(List<Card> deck) {
        this.deck = deck;
    }

    public Card getTrump() {
        return trump;
    }

    public void setTrump(Card trump) {
        this.trump = trump;
    }

    public int getNextPlayer() {
        return nextPlayer;
    }

    public void setNextPlayer(int nextPlayer) {
        this.nextPlayer = nextPlayer;
    }

    public int getCurrentRound() {
        return currentRound;
    }

    public void setCurrentRound(int currentRound) {
        this.currentRound = currentRound;
    }

    public int getCurrentNumberOfCardsDealt() {
        return currentNumberOfCardsDealt;
    }

    public void setCurrentNumberOfCardsDealt(int currentNumberOfCardsDealt) {
        this.currentNumberOfCardsDealt = currentNumberOfCardsDealt;
    }

    public int getCurrentNumberOfCardsPlayed() {
        return currentNumberOfCardsPlayed;
    }

    public void setCurrentNumberOfCardsPlayed(int currentNumberOfCardsPlayed) {
        this.currentNumberOfCardsPlayed = currentNumberOfCardsPlayed;
    }

    public Scoreboard getScoreboard() {
        return scoreboard;
    }

    public void setScoreboard(Scoreboard scoreboard) {
        this.scoreboard = scoreboard;
    }

    @Override
    public String toString() {
        return "Game{" +
                "numberOfPlayers=" + numberOfPlayers +
                ", players=" + players +
                ", numberOfRounds=" + numberOfRounds +
                ", deck=" + deck +
                ", trump=" + trump +
                ", nextPlayer=" + nextPlayer +
                ", currentRound=" + currentRound +
                ", currentNumberOfCardsDealt=" + currentNumberOfCardsDealt +
                ", currentNumberOfCardsPlayed=" + currentNumberOfCardsPlayed +
                ", scoreboard=" + scoreboard +
                ", playedCards=" + playedCards +
                '}';
    }

    public List<Card> computeCardsOnTheTable() {
        playedCards = new ArrayList<>();
        if(currentNumberOfCardsPlayed != 0) {
            int firstPLayer = currentRound % numberOfPlayers;
            int i = firstPLayer;
            int numberOfPlayedCards = 0;
            while (i < numberOfPlayers && numberOfPlayedCards < currentNumberOfCardsPlayed) {
                ++numberOfPlayedCards;
                playedCards.add(players.get(i).get(0));
                ++i;
            }
            i = 0;
            while (numberOfPlayedCards < currentNumberOfCardsPlayed) {
                ++numberOfPlayedCards;
                playedCards.add(players.get(i).get(0));
                ++i;
            }
        }

        return playedCards;
    }

    public boolean isPlayerHasToPressCard() {
        return playerHasToPressCard;
    }

    public void setPlayerHasToPressCard(boolean playerHasToPressCard) {
        this.playerHasToPressCard = playerHasToPressCard;
    }

    public Card getPressedCard() {
        return pressedCard;
    }

    public void setPressedCard(Card pressedCard) {
        this.pressedCard = pressedCard;
    }
}
