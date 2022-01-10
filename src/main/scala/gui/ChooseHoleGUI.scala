package gui
import java.awt.Dimension
import java.awt.FlowLayout
import javax.swing.{BoxLayout, JButton, JFrame, JLabel, JPanel, SwingConstants}
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.postfixOps
import concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import gameboard.KalahaBoard
import player.Player

import java.util.{Timer, TimerTask}

class ChooseHoleGUI(private val mainGUI: MainGUI, private val kalahaBoard: KalahaBoard, player: Player) {
  private val board: Array[Int] = kalahaBoard.getBoard()
  private val player1BaseIndex: Int = kalahaBoard.getPlayer1BaseIndex()
  private val player2BaseIndex: Int = kalahaBoard.getPlayer2BaseIndex()
  private var isHoleChosen: Boolean = false
  private val maxWaitTime: Int = 30000
  private var startHoleIndex: Int = -1
  private val jPanel = JPanel()
  private val columnJPanel = new JPanel()
  columnJPanel.setLayout(new BoxLayout(columnJPanel, BoxLayout.Y_AXIS))

  if player.getIsFirstPlayer() then {
    startHoleIndex = 0

    columnJPanel.add(new JLabel("Current game board"))
    columnJPanel.add(new JLabel("Enemy row"))

    val rowJPanel1 = new JPanel()
    rowJPanel1.setLayout(new BoxLayout(rowJPanel1, BoxLayout.X_AXIS))
    for (i <- player2BaseIndex - 1 to player1BaseIndex + 1 by -1) {
      rowJPanel1.add(new JLabel(board(i) + "   "))
    }

    columnJPanel.add(rowJPanel1)
    columnJPanel.add(new JLabel(board(player2BaseIndex).toString + " <- Enemy score | Your Score -> " + board(player1BaseIndex).toString))
    columnJPanel.add(new JLabel("Your row"))

    val rowJPanel2 = new JPanel()
    rowJPanel2.setLayout(new BoxLayout(rowJPanel2, BoxLayout.X_AXIS))
    for (i <- 0 to player1BaseIndex-1) {
      val jButton = JButton(board(i).toString)
      jButton.addActionListener(new SelectHole(jButton, i))
      rowJPanel2.add(jButton)
    }

    columnJPanel.add(rowJPanel2)
    columnJPanel.add(new JLabel("Choose hole"))

    jPanel.add(columnJPanel)
  }

  else {
    startHoleIndex = player1BaseIndex + 1

    columnJPanel.add(new JLabel("Current game board"))
    columnJPanel.add(new JLabel("Enemy row"))

    val rowJPanel1 = new JPanel()
    rowJPanel1.setLayout(new BoxLayout(rowJPanel1, BoxLayout.X_AXIS))
    for (i <- player1BaseIndex - 1 to 0 by -1) {
      rowJPanel1.add(new JLabel(board(i) + "   "))
    }

    columnJPanel.add(rowJPanel1)
    columnJPanel.add(new JLabel(board(player1BaseIndex).toString + " <- Enemy score | Your Score -> " + board(player2BaseIndex).toString))
    columnJPanel.add(new JLabel("Your row"))

    val rowJPanel2 = new JPanel()
    rowJPanel2.setLayout(new BoxLayout(rowJPanel2, BoxLayout.X_AXIS))
    for (i <- player1BaseIndex + 1 to player2BaseIndex - 1) {
      val jButton = JButton(board(i).toString)
      jButton.addActionListener(new SelectHole(jButton, i))
      rowJPanel2.add(jButton)
    }

    columnJPanel.add(rowJPanel2)
    columnJPanel.add(new JLabel("Choose hole"))

    jPanel.add(columnJPanel)
  }

  // Wait certain amount of time for user input
  private val waiter = new Timer().schedule(
    new TimerTask {
      override def run(): Unit =
        if !isHoleChosen then mainGUI.changeLayoutToLackOfActivity()
    },
    maxWaitTime
  )

  class SelectHole(var jButton: JButton, var index: Int) extends ActionListener {
    override def actionPerformed(event: ActionEvent): Unit = {
      if board(index) != 0 then {
        isHoleChosen = true
        mainGUI.getServer().usePlayerMove(index - startHoleIndex)
      }
    }
  }

  def getJPanel(): JPanel = jPanel
}
