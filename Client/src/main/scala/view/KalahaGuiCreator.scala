package view

import java.awt.{Container, Dimension, GridLayout}
import javax.swing.{JButton, JFrame, JPanel, JTextField, JTextPane}

class KalahaGuiCreator:
    /* window elements */
    val frame: JFrame = new JFrame("Kalaha game client")
    val contentPane: Container = frame.getContentPane()

    var controlsPanel: ControlsPanel = null
    var boardPanel: BoardPanel = null
    var statusPanel: StatusPanel = null

    def createControls(): Unit =
        controlsPanel = new ControlsPanel()
        contentPane.add(controlsPanel)

    def createBoard(): Unit =
        boardPanel = new BoardPanel()
        contentPane.add(boardPanel)
        
    def createStatus(): Unit =
        statusPanel = new StatusPanel()
        contentPane.add(statusPanel)
        
    def show(): Unit =    
        frame.setLayout(new GridLayout(2, 2, 10, 10))
        frame.setDefaultCloseOperation(3)  // EXIT_ON_CLOSE
        frame.setMinimumSize(new Dimension(900, 600))
        frame.setVisible(true)
