package kalaha.ui
import java.awt.Color
import scala.swing.Panel

class TransparentPanel extends Panel{
  peer.setOpaque(false)
  peer.setBackground(new Color(0,0,0,0))
}
