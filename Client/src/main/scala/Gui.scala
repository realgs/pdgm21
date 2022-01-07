import view.KalahaGuiCreator

import java.awt.{Dimension, GridLayout}
import javax.swing.{JButton, JFrame, JPanel, JTextField, JTextPane, SpringLayout}

object Gui:
    def main(args: Array[String]): Unit =
        val creator = new KalahaGuiCreator()
        creator.createControls()
        creator.createBoard()
        creator.createStatus()
        creator.show()