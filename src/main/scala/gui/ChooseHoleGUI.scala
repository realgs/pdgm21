package gui
import java.awt.Dimension
import java.awt.FlowLayout
import javax.swing.{BoxLayout, JButton, JFrame, JLabel, JPanel}
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import gameboard.KalahaBoard
import player.Player

class ChooseHoleGUI(private val mainGUI: MainGUI, private val kalahaBoard: KalahaBoard, player: Player) {
  private val board = kalahaBoard.getBoard()
  private val player1BaseIndex = kalahaBoard.getPlayer1BaseIndex()
  private val player2BaseIndex = kalahaBoard.getPlayer1BaseIndex()
  private val jPanel = JPanel()

  if player.getIsFirstPlayer() then {
    jPanel.add(new JLabel("Current game board"))
    jPanel.add(new JLabel("Enemy row"))
    for (i <- player2BaseIndex - 1 to player1BaseIndex + 1 by -1)
      jPanel.add(new JLabel(board(i).toString + " "))
    jPanel.add(new JLabel("Enemy score: " + board(player2BaseIndex).toString + "\n"))
    jPanel.add(new JLabel("Your row"))
    for (i <- 0 to player1BaseIndex-1) {
      val jButton: JButton = JButton(board(i).toString)
      jPanel.add(jButton)
    }
    jPanel.add(new JLabel("\nYour score: " + board(player1BaseIndex).toString + "\n"))
  }




  def getJPanel(): JPanel = jPanel
}
