package gui
import javax.swing.{JFrame, JPanel}
import java.awt.{BorderLayout, CardLayout, Dimension, FlowLayout}
import server.Server
import gui.ChooseBoardSizeGUI

class MainGUI(var server: Server) {
  private var boardSize: Int = 0
  private var noStartingStones: Int = 0
  private val jFrame = JFrame()

  jFrame.setTitle("Kalaha game")
  jFrame.setMinimumSize(new Dimension(900, 450))
  jFrame.setLayout(new FlowLayout())
  changeLayoutToChooseBoardSize()
  jFrame.pack()
  jFrame.setVisible(true)

  def changeLayoutToChooseBoardSize(): Unit = {
    val gui = new ChooseBoardSizeGUI(this)
    val panel = gui.getJPanel()
    jFrame.getContentPane().removeAll()
    jFrame.getContentPane().add(panel)
    jFrame.revalidate()
  }

  def changeLayoutToChooseNumberOfStones(boardSize: Int): Unit = {
    this.boardSize = boardSize
    val gui = new ChooseNoStartingStonesGUI(this)
    val panel = gui.getJPanel()
    jFrame.getContentPane().removeAll()
    jFrame.getContentPane().add(panel)
    jFrame.revalidate()
  }

  def changeLayoutToChoosePlayer(noStartingStones: Int): Unit = {
    this.noStartingStones = noStartingStones
    val gui = new ChoosePlayers(this)
    val panel = gui.getJPanel()
    jFrame.getContentPane().removeAll()
    jFrame.getContentPane().add(panel)
    jFrame.revalidate()
  }

  def changeLayoutToShowGame(isHuman1: Boolean, isHuman2: Boolean): Unit = {
    server.initializeGame(boardSize, noStartingStones, isHuman1, isHuman2)
    val gui = new ChoosePlayers(this)
    val panel = gui.getJPanel()
    jFrame.getContentPane().removeAll()
    jFrame.getContentPane().add(panel)
    jFrame.revalidate()
  }
}
