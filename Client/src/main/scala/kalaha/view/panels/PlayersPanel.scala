package kalaha.view.panels

import kalaha.connection.Connection

import java.awt.{Color, FlowLayout}
import javax.swing.border.LineBorder
import javax.swing.{JButton, JLabel, JPanel}

class PlayersPanel extends JPanel:
    var showPlayersButton: JButton = new JButton("Show players")
    var firstPlayerLabel: JLabel = new JLabel("AliceAI")
    var secondPlayerLabel: JLabel = new JLabel("JohnnyAI")

    showPlayersButton.setEnabled(false)
    
    add(showPlayersButton)
    add(firstPlayerLabel)
    add(secondPlayerLabel)
    setLayout(new FlowLayout(FlowLayout.CENTER, 10, 10))