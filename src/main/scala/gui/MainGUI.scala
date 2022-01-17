package gui
import javax.swing.{JFrame}
import java.awt.{Dimension, FlowLayout}
import server.Server
import gui.ChooseBoardSizeGUI
import player.Player

class MainGUI(var server: Server) {
  private val jFrame = JFrame()
  jFrame.setTitle("Kalaha game")
  jFrame.setMinimumSize(new Dimension(900, 450))
  jFrame.setLayout(new FlowLayout())
  changeLayoutToChooseBoardSize()
  jFrame.pack()
  jFrame.setVisible(true)

  def getServer(): Server = server

  def changeLayoutToChooseBoardSize(): Unit =
    val gui = new ChooseBoardSizeGUI(this)
    val panel = gui.getJPanel()
    jFrame.getContentPane().removeAll()
    jFrame.getContentPane().add(panel)
    jFrame.revalidate()
    jFrame.repaint()

  def changeLayoutToChooseNumberOfStones(): Unit =
    val gui = new ChooseNoStartingStonesGUI(this)
    val panel = gui.getJPanel()
    jFrame.getContentPane().removeAll()
    jFrame.getContentPane().add(panel)
    jFrame.revalidate()
    jFrame.repaint()

  def changeLayoutToChoosePlayer(): Unit =
    val gui = new ChoosePlayersGUI(this)
    val panel = gui.getJPanel()
    jFrame.getContentPane().removeAll()
    jFrame.getContentPane().add(panel)
    jFrame.revalidate()
    jFrame.repaint()

  def startGame(): Unit =
    server.initializeGame()
    server.playGame()

  def changeLayoutToChooseHole(player: Player): Unit =
    val gui = new ChooseHoleGUI(this, server.getKalahaBoard(), player)
    val panel = gui.getJPanel()
    jFrame.getContentPane().removeAll()
    jFrame.getContentPane().add(panel)
    jFrame.revalidate()
    jFrame.repaint()

  def changeLayoutToShowResults(): Unit =
    val gui = new ShowResultsGUI(this)
    val panel = gui.getJPanel()
    jFrame.getContentPane().removeAll()
    jFrame.getContentPane().add(panel)
    jFrame.revalidate()
    jFrame.repaint()

  def changeLayoutToLackOfActivity(): Unit =
    val gui = new LackOfActivityGUI(this)
    val panel = gui.getJPanel()
    jFrame.getContentPane().removeAll()
    jFrame.getContentPane().add(panel)
    jFrame.revalidate()
    jFrame.repaint()
}
