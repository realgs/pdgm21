package kalaha.ui

import kalaha.models.PlayerEnum
import kalaha.models.PlayerEnum.{First, Second,  Player}
import kalaha.utils.Constants.*

import java.awt.{Color, Dimension}
import javax.swing.Box
import scala.swing.event.ButtonClicked
import scala.swing.*

abstract class BattleView(player: Player) extends MiddleView {

  val firstPlayerButtonArray: Array[Button] = Array.fill(PLAYER_HOLES+1)(new Button("%2d".format(STONES_AMOUNT)))
  val secondPlayerButtonArray: Array[Button] = Array.fill(PLAYER_HOLES+1)(new Button("%2d".format(STONES_AMOUNT)))
  firstPlayerButtonArray(PLAYER_HOLES).text = "%2d".format(0)
  secondPlayerButtonArray(PLAYER_HOLES).text = "%2d".format(0)

  protected val firstPlayerButtons = new GridPanel(1, PLAYER_HOLES+1)
  protected val secondPlayerButtons = new GridPanel(1, PLAYER_HOLES+1)

  firstPlayerButtons.opaque = opaqueSetting
  firstPlayerButtons.background = opaqueColor
  secondPlayerButtons.opaque = opaqueSetting
  secondPlayerButtons.background = opaqueColor


  firstPlayerButtons.contents += TransparentPanel()
  secondPlayerButtons.contents += TransparentPanel()

  player match
    case First =>
      for(i <- 0 until PLAYER_HOLES){
        firstPlayerButtons.contents += firstPlayerButtonArray(i)
        secondPlayerButtons.contents += secondPlayerButtonArray(PLAYER_HOLES - i)
      }
      secondPlayerButtons.contents += secondPlayerButtonArray(0)
    case Second =>
      for(i <- 0 until PLAYER_HOLES){
        firstPlayerButtons.contents += firstPlayerButtonArray(PLAYER_HOLES - i)
        secondPlayerButtons.contents += secondPlayerButtonArray(i)
      }
      firstPlayerButtons.contents += firstPlayerButtonArray(0)


  secondPlayerButtons.contents += TransparentPanel()
  firstPlayerButtons.contents += TransparentPanel()


  val firstMacala = firstPlayerButtonArray(PLAYER_HOLES)
  val secondMacala = secondPlayerButtonArray(PLAYER_HOLES)

  firstMacala.enabled = false
  secondMacala.enabled = false

  val centerLabel = new Label()

  val macalasPanel: BorderPanel


}
