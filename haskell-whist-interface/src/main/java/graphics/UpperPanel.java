package graphics;

import javax.swing.*;
import java.awt.event.ActionEvent;

public class UpperPanel extends JPanel {
    final MainFrame frame;
    JLabel label;
    JSpinner spinner;
    JButton createButton;

    public UpperPanel(MainFrame frame) {
        this.frame = frame;
        init();
    }

    private void init() {
        label = new JLabel("Number of players:");
        spinner = new JSpinner(new SpinnerNumberModel(3, 3, 6, 1));
        createButton = new JButton("Start Game");
        createButton.addActionListener(this::updateVisualsAndStartGame);

        add(label);
        add(spinner);
        add(createButton);
    }

    private void updateVisualsAndStartGame(ActionEvent actionEvent) {
        frame.canvas.init((Integer) spinner.getValue());
        SwingUtilities.updateComponentTreeUI(frame);
        repaint();
        frame.game.playGame((Integer) spinner.getValue());
    }
}
