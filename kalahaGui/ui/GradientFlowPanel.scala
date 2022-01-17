package kalaha.ui

import java.awt.{Color, GradientPaint, Graphics, Graphics2D, RenderingHints}
import scala.swing.{Component, FlowPanel, SequentialContainer, Panel}
import kalaha.models.GradientTypeEnum.*

class BackgroundGradPanel(val color1: Color = Color.BLUE, val color2: Color = Color.WHITE, val gradType: GradType = VERTICAL) (contents0: Component*) extends FlowPanel {

  def this(contents0: Component*) = this(color1 = Color.BLUE, color2 = Color.WHITE, gradType = VERTICAL) (contents0: _*)

  contents ++= contents0

  override protected def paintComponent(g: Graphics2D): Unit =
    super.paintComponent(g)
    val height = peer.getHeight
    val width = peer.getWidth
    g.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)

    gradType match
      case VERTICAL =>
        g.setPaint( GradientPaint(0, 0, color1, 0 , height, color2) )
      case HORIZONTAL =>
        g.setPaint( GradientPaint(0, height/2, color1, width, height/2, color2) )
      case DIAGONAL_TOP =>
        g.setPaint( GradientPaint(0, 0, color1, width, height, color2) )
      case DIAGONAL_DOWN =>
        g.setPaint( GradientPaint(0, height, color1, width, 0, color2) )

    g.fillRect(0, 0, width, height)
}
