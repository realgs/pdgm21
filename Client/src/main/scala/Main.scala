import controller.KalahaClientConnection
import status.GameStatus
import view.KalahaGuiCreator

import java.awt.{Dimension, GridLayout}
import javax.swing.{JButton, JFrame, JPanel, JTextField, JTextPane, SpringLayout}

object Main:
    def main(args: Array[String]): Unit =
        val gameStatus: GameStatus = new GameStatus()
        val gui: KalahaGuiCreator = new KalahaGuiCreator()
        gui.createBoard()
        gui.createControls()
        gui.show()

        val connection: KalahaClientConnection = new KalahaClientConnection(gui, gameStatus)