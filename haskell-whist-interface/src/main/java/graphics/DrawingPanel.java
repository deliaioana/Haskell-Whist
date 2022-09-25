package graphics;

import game.Card;
import game.Game;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.List;

public class DrawingPanel extends JPanel {
    private final MainFrame frame;
    int canvasWidth = 800, canvasHeight = 600;
    int numberOfPlayers;
    int padX = 50;
    int padY = 50;
    int playerHeight = 100;
    int tableHeight = 150;
    int tableWidth = 400;
    int playerPadding = 50;
    int playerWidth = 100;
    int cardWidth = 50;
    int cardHeight = 70;
    int cardSpacing = 10;
    int hiddenCardWidth = 10;
    int hiddenCardHeight = 15;

    public DrawingPanel(MainFrame frame) {
        this.frame = frame;
        init(numberOfPlayers);
    }

    final void init(int numberOfPlayers) {
        setPreferredSize(new Dimension(canvasWidth, canvasHeight));
        setNumberOfPlayers(numberOfPlayers);

        addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                System.out.println("S-a apasat click!");
                if(frame.getGame().getNextPlayer() == 1) {
                    if(!frame.getGame().getPlayers().isEmpty() && !frame.getGame().getPlayers().get(0).isEmpty()){
                        int count = frame.getGame().getPlayers().get(0).size();
                        int totalWidth = cardWidth * count + (count - 1) * cardSpacing;
                        int x = canvasWidth/2 - (totalWidth/2);
                        int y = 500;
                        for (Card card : frame.getGame().getPlayers().get(0)) {
                            if(areCoordinatesInsideRectangle(e.getX(), e.getY(), x, y, x+cardWidth, y+cardHeight)) {
                                System.out.println("cardul apasat este: " + card);
                                frame.getGame().setPressedCard(card);
                                frame.getGame().setPlayerHasToPressCard(false);
                            }
                            x += cardWidth + cardSpacing;
                        }
                    }
                }
            }
        });
    }

    private boolean areCoordinatesInsideRectangle(int x, int y, int x1, int y1, int x2, int y2) {
        return x > x1 && x < x2 && y > y1 && y < y2;
    }

    private void setNumberOfPlayers(int x) {
        numberOfPlayers = x;
    }

    @Override
    protected void paintComponent(Graphics graphics) {
        System.out.println("Hello");
        Graphics2D g = (Graphics2D) graphics;
        g.setColor(Color.DARK_GRAY);
        g.fillRect(0, 0, canvasWidth, canvasHeight);
        if(frame.getGame() != null) {
            paintGame(g, frame.getGame());
        }
    }

    private void paintGame(Graphics2D g, Game game) {
        paintBackground(g, numberOfPlayers);
        if(game.getPlayers() != null) paintCards(g, game);
        paintTrump(g, game.getTrump());
        paintNextPlayerAnnouncer(g, game.getNextPlayer());
        paintCardsOnTable(g, game.computeCardsOnTheTable(), game);
    }

    private void paintCardsOnTable(Graphics2D g, List<Card> cards, Game game) {
        int x = getXCoordinateForTableCards(game.getNumberOfPlayers());
        for(int i = cards.size()-1; i >= 0; --i) {
            paintCard(g, cards.get(i), x, padY + playerHeight + padY + (tableHeight - cardHeight) / 2);
        }
    }

    private int getXCoordinateForTableCards(int numberOfPlayers) {
        int width = cardWidth * numberOfPlayers + (cardSpacing * (numberOfPlayers-1));
        int spareSpace = tableWidth - width;
        return padX + 150 + spareSpace / 2;
    }

    private void paintNextPlayerAnnouncer(Graphics2D g, int nextPlayer) {
        g.setColor(Color.WHITE);
        String announcement = "Next player: ";
        if(nextPlayer == 1) {
            announcement = announcement + " YOU";
        }
        else {
            announcement = announcement + " " + nextPlayer;
        }
        g.drawString(announcement, 650, 225);
    }

    private void paintTrump(Graphics2D g, Card trump) {
        g.setColor(Color.WHITE);
        if(trump != null) {
            g.drawString("Trump card: ", 70, 225);
            paintCard(g, trump, 70, 250);
        }
        else {
            g.drawString("No trump", 70, 270);
        }
    }

    private void paintCard(Graphics2D g, Card card, int x, int y) {
        if(card.getSign() != '0') {
            g.setColor(Color.WHITE);
            g.fillRect(x, y, cardWidth, cardHeight);
            //acest rect trb sa fie button
            g.setColor(Color.BLACK);
            String signToPaint;
            if(card.getSign() == 'C') {
                signToPaint = "♣";
            } else if (card.getSign() == 'H') {
                g.setColor(Color.RED);
                signToPaint = "♥";
            } else if (card.getSign() == 'D') {
                g.setColor(Color.RED);
                signToPaint = "♦";
            } else {
                signToPaint = "♠";
            }
            g.drawString(signToPaint, x+5, y+15);
            g.drawString(signToPaint, x+37, y+63);

            g.drawString(card.getValueToPaint(), x+22, y+37);
        }

    }

    private void paintCards(Graphics2D g, Game game) {
        printUserCards(g, game);
        for(int i = 1; i < game.getNumberOfPlayers(); ++i) {
            printHiddenPlayerCards(g, i, game);
        }
    }

    private void printUserCards(Graphics2D g, Game game) {
        g.setColor(Color.WHITE);
        g.drawString("Your cards:", 70, 475);
        if(!game.getPlayers().isEmpty()) {
            int count = game.getPlayers().get(0).size();
            int totalWidth = cardWidth * count + (count - 1) * cardSpacing;
            int x = canvasWidth/2 - (totalWidth/2);
            int y = 500;
            for (Card card : game.getPlayers().get(0)) {
                paintCard(g, card, x, y);
                x += cardWidth + cardSpacing;
            }
        }
    }

    private void printHiddenPlayerCards(Graphics2D g, int i, Game game) {
        int count = game.getPlayers().get(i).size();
        int totalWidth = hiddenCardWidth * count + (count-1) * 5;
        int startingX = getXCoordinateFromPlayerI(game.getNumberOfPlayers()-1, i-1);
        for(int j = 1; j <= count; ++j) {
            int x = startingX + hiddenCardWidth * (j-1) + (j-1) * 5;;
            int y = 160;
            drawHiddenCard(g, x, y);
        }
    }

    private int getXCoordinateFromPlayerI(int number, int i) {
        int playersSpotWidth = (number-1) * playerPadding + number * playerWidth;
        int x = canvasWidth/2 - (playersSpotWidth)/2;
        return x + i * (playerWidth + playerPadding);
    }

    private void drawHiddenCard(Graphics2D g, int x, int y) {
        g.setColor(Color.BLACK);
        g.fillRect(x, y, hiddenCardWidth, hiddenCardHeight);
    }

    private void paintBackground(Graphics2D g, int numberOfPlayers) {
        paintOpponents(g, numberOfPlayers-1);
        paintTable(g);
    }

    private void paintTable(Graphics2D g) {
        int x1 = padX + 150;
        int y1 = padY + playerHeight + padY;
        g.setColor(Color.GRAY);
        g.fillRect(x1, y1, tableWidth, tableHeight);
    }

    private void paintOpponents(Graphics2D g, int number) {
        int y = padY;
        int playersSpotWidth = (number-1) * playerPadding + number * playerWidth;
        int x = canvasWidth/2 - (playersSpotWidth)/2;
        for(int i = 0; i < number; ++i) {
            paintOpponent(g, x, y, i);
        }
    }

    private void paintOpponent(Graphics g, int x, int y, int i) {
        g.setColor(Color.BLACK);
        int x1 = x + i*(playerWidth + playerPadding);
        g.fillOval(x1 + playerWidth/4, y, playerWidth/2, playerWidth/2);
        g.fillRoundRect(x1, y + playerHeight/2,  playerWidth, playerHeight/2, 20, 20);
    }

    @Override
    public void update(Graphics g) { } //No need for update
}