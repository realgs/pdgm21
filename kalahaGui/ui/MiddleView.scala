package kalaha.ui

import kalaha.utils.Constants.{PREFERED_HEIGHT, PREFERED_WIDTH}

import java.awt.{Color, Dimension}
import scala.swing.Panel

trait MiddleView {

  protected val opaqueSetting = false
  protected val opaqueColor = new Color(0,0,0,0)

  protected val topPanel = new TransparentPanel()
  protected val bottomPanel = new TransparentPanel()
  topPanel.preferredSize = Dimension(PREFERED_WIDTH, 200)
  bottomPanel.preferredSize = Dimension(PREFERED_WIDTH, 200)

  protected val leftPanel = new TransparentPanel()
  protected val rightPanel = new TransparentPanel()
  leftPanel.preferredSize = Dimension(200, PREFERED_HEIGHT)
  rightPanel.preferredSize = Dimension(200, PREFERED_HEIGHT)
  

  def getView(): BackgroundGradPanel
}
