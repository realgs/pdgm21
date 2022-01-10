package gui
import javax.swing.{JLabel, JPanel}

class LackOfActivityGUI(private val mainGUI: MainGUI) {

  private val jPanel = JPanel()
  jPanel.add(new JLabel("Game stopped due to lack of activity from one of the players"))

  def getJPanel(): JPanel = jPanel
}
