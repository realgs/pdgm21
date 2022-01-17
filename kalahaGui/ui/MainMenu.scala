package kalaha.ui

import kalaha.models.GradientTypeEnum.HORIZONTAL
import kalaha.utils.Constants.*
import kalaha.resource.strings.*

import javax.swing.Box
import java.awt.{Color, Dimension}
import scala.swing.event.ButtonClicked
import scala.swing.{Alignment, BorderPanel, BoxPanel, Button, FlowPanel, GridPanel, Label, Orientation, Panel, Reactions, Separator, Swing}

object MainMenu extends MiddleView {

  val PvP_Btn = new Button(pvp_game)
  val PvC_Btn = new Button(pvc_game)
  val CvC_Btn = new Button(cvc_game)
  val about_Btn = new Button(about)

  private val titleLabel = new Label(app_name)
  titleLabel.opaque = opaqueSetting
  titleLabel.background = opaqueColor
  titleLabel.horizontalTextPosition = Alignment.Center
  titleLabel.horizontalAlignment = Alignment.Center

  private val grid = new GridPanel(4, 1) {
    contents += MainMenu.PvP_Btn
    contents += MainMenu.PvC_Btn
    contents += MainMenu.CvC_Btn
    contents += MainMenu.about_Btn
  }
  grid.vGap = 10
  grid.opaque = opaqueSetting
  grid.background = opaqueColor

  val box =  new BoxPanel(Orientation.Vertical){
    contents += titleLabel
    contents += Swing.VStrut(10)
    contents += Swing.Glue
    contents += grid
  }
  box.opaque = opaqueSetting
  box.background = opaqueColor

  private val mainMenu2 = BackgroundGradPanel(gradType = HORIZONTAL)(
    Swing.VStrut(2*PREFERED_HEIGHT/3),
    box
  )

  box.contents 

  def getView(): BackgroundGradPanel =
    mainMenu2


  /*
    Should not use, experimental during creation process, works... but not looks as goot during resizing
    Status: @Deprecated

    def getView(): BackgroundGradBorderPanel =
    mainMenu

  private val mainMenu = new BackgroundGradBorderPanel(gradType = HORIZONTAL)

  private val menu = new BoxPanel(Orientation.Vertical){
    contents += new Label(app_name)
    contents += Swing.VStrut(10)
    contents += Swing.Glue
    val grid = new GridPanel(4, 1) {
      contents += PvP_Btn
      contents += PvC_Btn
      contents += CvC_Btn
      contents += about_Btn
    }
    grid.vGap = 10
    grid.opaque = false
    grid.background = new Color(0,0,0,0)
    contents += grid
  }

  menu.opaque = false
  menu.background = new Color(0,0,0,0)

  mainMenu.addComponent(topPanel, BorderPanel.Position.North)
  mainMenu.addComponent(bottomPanel, BorderPanel.Position.South)
  mainMenu.addComponent(leftPanel, BorderPanel.Position.East)
  mainMenu.addComponent(rightPanel, BorderPanel.Position.West)

  mainMenu.addComponent(menu, BorderPanel.Position.Center)

  */



}
