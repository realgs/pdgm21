import controller.KalahaClientConnection
import view.KalahaGuiCreator

import java.awt.{Dimension, GridLayout}
import javax.swing.{JButton, JFrame, JPanel, JTextField, JTextPane, SpringLayout}

object Main:
    def main(args: Array[String]): Unit =
        val gui: KalahaGuiCreator = new KalahaGuiCreator()
        gui.createBoard()
        gui.createControls()
        gui.show()

        val connection: KalahaClientConnection = new KalahaClientConnection(gui)