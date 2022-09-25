package graphics;

import game.Game;

import javax.swing.*;
import java.awt.*;

public class MainFrame extends JFrame {
    UpperPanel configPanel;
    DrawingPanel canvas;
    Game game = new Game(this);

    public MainFrame() {
        super("Haskell Whist");
        init();
    }

    public DrawingPanel getCanvas() {
        return canvas;
    }

    public Game getGame() {
        return game;
    }

    public void setGame(Game g) { game = g; }

    private void init() {
        setDefaultCloseOperation(EXIT_ON_CLOSE);
        configPanel = new UpperPanel(this);
        canvas = new DrawingPanel(this);

        add(canvas, BorderLayout.CENTER);
        add(configPanel, BorderLayout.NORTH);

        pack();
    }

    public void refreshCanvas() {
        repaint();
        getCanvas().paintImmediately(0, 0, getCanvas().getWidth(), getCanvas().getHeight());
    }
}
