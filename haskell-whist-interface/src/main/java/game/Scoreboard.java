package game;

import netscape.javascript.JSObject;
import org.json.JSONArray;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.List;

public class Scoreboard {
    private int numberOfPlayers;
    private List<String> playerNames;
    private List<Integer> playerPoints;
    private List<ScoreLine> scoreLines;

    public Scoreboard(int numberOfPlayers, String playerName) {
        setNumberOfPlayers(numberOfPlayers);
        List<String> names = new ArrayList<>();
        names.add(playerName);
        StringBuilder botName = new StringBuilder("P");
        for(int i = 1; i < numberOfPlayers; ++i) {
            StringBuilder newBot = new StringBuilder(botName.toString());
            newBot.append(i+1);
            names.add(newBot.toString());
        }
        setPlayerNames(names);
    }

    public void setPlayerNames(List<String> playerNames) {
        this.playerNames = playerNames;
    }

    public void setNumberOfPlayers(int numberOfPlayers) {
        this.numberOfPlayers = numberOfPlayers;
    }

    public List<Integer> getPlayerPoints() {
        return playerPoints;
    }

    public void setPlayerPoints(List<Integer> playerPoints) {
        this.playerPoints = playerPoints;
    }

    @Override
    public String toString() {
        return "Scoreboard{" +
                "numberOfPlayers=" + numberOfPlayers +
                ", playerNames=" + playerNames +
                ", playerPoints=" + playerPoints +
                ", scoreLines=" + scoreLines +
                '}';
    }

    public JSONObject toJSON() {
        JSONObject scoreboardObject = new JSONObject();
        JSONArray scoreLines = new JSONArray();
        JSONArray contents = new JSONArray();
        for(int i = 0; i < numberOfPlayers; ++i) {
            contents.put(playerNames.get(i));
            contents.put(playerPoints.get(i));
        }
        contents.put(scoreLines);
        scoreboardObject.put("contents", contents);
        scoreboardObject.put("tag", "S"+numberOfPlayers);
        return scoreboardObject;
    }
}
