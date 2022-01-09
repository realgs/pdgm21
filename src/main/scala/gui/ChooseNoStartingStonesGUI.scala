package gui
import java.awt.Dimension
import java.awt.FlowLayout
import javax.swing.{BoxLayout, JButton, JFrame, JLabel, JPanel}
import java.awt.event.ActionListener
import java.awt.event.ActionEvent

class ChooseNoStartingStonesGUI(private val mainGUI: MainGUI) {
  private val jPanel = JPanel()
  private val jLabel = JLabel("Choose starting number of stones")
  private val jButtonBoardSize1 = JButton("3")
  private val jButtonBoardSize2 = JButton("4")
  private val jButtonBoardSize3 = JButton("5")
  private val jButtonBoardSize4 = JButton("6")
  private val jButtonBoardSize5 = JButton("7")

  jButtonBoardSize1.addActionListener(new SelectOption(jButtonBoardSize1))
  jButtonBoardSize2.addActionListener(new SelectOption(jButtonBoardSize2))
  jButtonBoardSize3.addActionListener(new SelectOption(jButtonBoardSize3))
  jButtonBoardSize4.addActionListener(new SelectOption(jButtonBoardSize4))
  jButtonBoardSize5.addActionListener(new SelectOption(jButtonBoardSize5))

  jPanel.add(jLabel)
  jPanel.add(jButtonBoardSize1)
  jPanel.add(jButtonBoardSize2)
  jPanel.add(jButtonBoardSize3)
  jPanel.add(jButtonBoardSize4)
  jPanel.add(jButtonBoardSize5)

  class SelectOption(var jButton: JButton) extends ActionListener {
    override def actionPerformed(event: ActionEvent): Unit = {
      mainGUI.changeLayoutToChoosePlayer(jButton.getActionCommand().toInt)
    }
  }

  def getJPanel(): JPanel = jPanel
}
