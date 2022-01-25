package kalaha.view

import kalaha.connection.Connection
import kalaha.view.panels.{BoardPanel, ControlsPanel}

import java.awt.{Color, Container, Dimension, FlowLayout, GridLayout}
import javax.swing.{JButton, JFrame, JPanel, JTextField, JTextPane, SpringLayout, WindowConstants}

class GuiCreator:
    val frame: JFrame = new JFrame("Kalaha game client")
    val contentPane: Container = frame.getContentPane()

    var controlsPanel: ControlsPanel = null
    var boardPanel: BoardPanel = null

    def createControls(): Unit =
        controlsPanel = new ControlsPanel()
        contentPane.add(controlsPanel)

    def createBoard(): Unit =
        boardPanel = new BoardPanel()
        contentPane.add(boardPanel)

    def show(): Unit =
        contentPane.setLayout(new FlowLayout(FlowLayout.CENTER, 10, 10))
        frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
        frame.setMinimumSize(new Dimension(1300, 400))
        frame.setVisible(true)
