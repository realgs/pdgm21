package view

import controller.KalahaClientConnection

import java.awt.{Color, Container, Dimension, FlowLayout, GridLayout}
import javax.swing.{JButton, JFrame, JPanel, JTextField, JTextPane, SpringLayout}

class KalahaGuiCreator(conn: KalahaClientConnection):
    var connection = conn
    /* window elements */
    val frame: JFrame = new JFrame("Kalaha game client")
    val contentPane: Container = frame.getContentPane()

    var controlsPanel: ControlsPanel = null
    var boardPanel: BoardPanel = null

    def createControls(): Unit =
        controlsPanel = new ControlsPanel(connection)
        contentPane.add(controlsPanel)

    def createBoard(): Unit =
        boardPanel = new BoardPanel()
        contentPane.add(boardPanel)

    def show(): Unit =
        contentPane.setLayout(new FlowLayout(FlowLayout.CENTER, 10, 10))
        contentPane.setBackground(new Color(0x99BBFF))
        frame.setDefaultCloseOperation(3)  // EXIT_ON_CLOSE
        frame.setMinimumSize(new Dimension(1200, 500))
        frame.setVisible(true)
