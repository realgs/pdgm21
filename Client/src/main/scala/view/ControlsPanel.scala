package view

import controller.KalahaClientConnection

import java.awt.{Color, FlowLayout, GridLayout}
import javax.swing.border.LineBorder
import javax.swing.{JButton, JLabel, JPanel, JTextField, SwingConstants, SwingContainer}

class ControlsPanel(connection: KalahaClientConnection) extends JPanel:
    var connectionPanel: ConnectionPanel = new ConnectionPanel()
    var playersPanel: PlayersPanel = new PlayersPanel()
    var namePanel: NamePanel = new NamePanel()
    var gamePanel: GamePanel = new GamePanel()

    add(connectionPanel)
    add(playersPanel)
    add(namePanel)
    add(gamePanel)

    setLayout(new GridLayout(1, 4, 10, 10))
    setBorder(new LineBorder(Color.CYAN, 3))
