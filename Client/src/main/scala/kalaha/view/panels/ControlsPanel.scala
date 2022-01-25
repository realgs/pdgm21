package kalaha.view.panels

import kalaha.connection.Connection

import java.awt.{Color, FlowLayout, GridLayout}
import javax.swing.border.LineBorder
import javax.swing.*

class ControlsPanel extends JPanel:
    var connectionPanel: ConnectionPanel = new ConnectionPanel()
    var playersPanel: PlayersPanel = new PlayersPanel()
    var namePanel: NamePanel = new NamePanel()
    var gamePanel: GamePanel = new GamePanel()

    add(connectionPanel)
    add(playersPanel)
    add(namePanel)
    add(gamePanel)

    setLayout(new GridLayout(1, 4, 10, 10))
