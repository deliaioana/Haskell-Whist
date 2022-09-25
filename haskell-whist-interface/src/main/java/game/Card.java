package game;

import org.json.JSONArray;

public class Card {
    private int value;
    private char sign;

    public Card(int value, char sign) {
        setValue(value);
        setSign(sign);
    }

    public Card() {
    }

    public String getValueToPaint() {
        if(value <=10 && value > 1) {
            return String.valueOf(value);
        }
        else if(value == 11) {
                return "J";
            }
            else if(value == 12) {
                    return "Q";
                } else if (value == 13) {
                    return "K";
                } else return "A";
            }

    public void setValue(int value) {
        this.value = value;
    }

    public char getSign() {
        return sign;
    }

    public void setSign(char sign) {
        this.sign = sign;
    }

    @Override
    public String toString() {
        return "Card{" +
                "value=" + value +
                ", sign=" + sign +
                '}';
    }

    public JSONArray toJSON() {
        JSONArray cardJSON = new JSONArray();
        cardJSON.put(value);
        cardJSON.put(String.valueOf(sign));
        return cardJSON;
    }
}
