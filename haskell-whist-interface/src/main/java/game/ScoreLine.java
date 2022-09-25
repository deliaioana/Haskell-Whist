package game;

import java.util.List;

public class ScoreLine {
    private List<Integer> values;

    public List<Integer> getValues() {
        return values;
    }

    @Override
    public String toString() {
        return "ScoreLine{" +
                "values=" + values +
                '}';
    }
}
