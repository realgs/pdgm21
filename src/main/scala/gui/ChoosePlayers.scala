package gui
import java.awt.Dimension
import java.awt.FlowLayout
import javax.swing.{BoxLayout, JButton, JFrame, JLabel, JPanel}
import java.awt.event.ActionListener
import java.awt.event.ActionEvent

class ChoosePlayers(private val mainGUI: MainGUI) {
  private val jPanel = JPanel()
  private val jLabel = JLabel("Choose opponents (note AI/AI will end very quickly)")
  private val jButtonBoardSize1 = JButton("Human/Human")
  private val jButtonBoardSize2 = JButton("Human/AI")
  private val jButtonBoardSize3 = JButton("AI/Human")
  private val jButtonBoardSize4 = JButton("AI/AI")

  jButtonBoardSize1.addActionListener(new SelectOption(jButtonBoardSize1))
  jButtonBoardSize2.addActionListener(new SelectOption(jButtonBoardSize2))
  jButtonBoardSize3.addActionListener(new SelectOption(jButtonBoardSize3))
  jButtonBoardSize4.addActionListener(new SelectOption(jButtonBoardSize4))

  jPanel.add(jLabel)
  jPanel.add(jButtonBoardSize1)
  jPanel.add(jButtonBoardSize2)
  jPanel.add(jButtonBoardSize3)
  jPanel.add(jButtonBoardSize4)

  class SelectOption(var jButton: JButton) extends ActionListener {
    override def actionPerformed(event: ActionEvent): Unit = {
      jButton.getActionCommand() match {
        case "Human/Human" => {
          mainGUI.getServer().setIsHuman1(true)
          mainGUI.getServer().setIsHuman2(true)
        }
        case "Human/AI" => {
          mainGUI.getServer().setIsHuman1(true)
          mainGUI.getServer().setIsHuman2(false)
        }
        case "AI/Human" => {
          mainGUI.getServer().setIsHuman1(false)
          mainGUI.getServer().setIsHuman2(true)
        }
        case "AI/AI" => {
          mainGUI.getServer().setIsHuman1(false)
          mainGUI.getServer().setIsHuman2(false)
        }
      }

      mainGUI.startGame()
    }
  }

  def getJPanel(): JPanel = jPanel
}
