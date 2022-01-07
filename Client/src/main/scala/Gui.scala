import controller.KalahaClientConnection
import view.KalahaGuiCreator

import java.awt.{Dimension, GridLayout}
import javax.swing.{JButton, JFrame, JPanel, JTextField, JTextPane, SpringLayout}

object Gui:
    def main(args: Array[String]): Unit =
        val connection = new KalahaClientConnection()
        val creator = new KalahaGuiCreator(connection)
        creator.createBoard()
        creator.createControls()
        creator.show()