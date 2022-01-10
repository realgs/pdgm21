package gui
import java.awt.Dimension
import java.awt.FlowLayout
import javax.swing.{BoxLayout, JLabel, JPanel}

class ShowResultsGUI(private val mainGUI: MainGUI) {
  private val (player1Results, player2Results): (Int, Int) = mainGUI.getServer().getKalahaBoard().getResults()
  private val jPanel = JPanel()
  jPanel.setLayout(new BoxLayout(jPanel, BoxLayout.Y_AXIS))
  jPanel.add(new JLabel("Player 1 final score: " + player1Results.toString))
  jPanel.add(new JLabel("Player 2 final score: " + player2Results.toString))
  if player1Results == player2Results then jPanel.add(new JLabel("Game ended in tie"))
  else if player1Results > player2Results then jPanel.add(new JLabel("Player 1 won"))
  else jPanel.add(new JLabel("Player 2 won"))

  def getJPanel(): JPanel = jPanel
}
