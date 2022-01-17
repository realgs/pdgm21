package kalaha.ui

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import kalaha.models.{GUIEventsHandler, GameBoard}
import kalaha.models.GradientTypeEnum.HORIZONTAL
import kalaha.models.PlayerEnum.{First, Player, Second}
import kalaha.resource.strings.{first_player_turn, second_player_turn}
import kalaha.utils.Constants.{PLAYER_HOLES, PREFERED_HEIGHT}

import scala.swing.{Action, BorderPanel, FlowPanel, GridPanel, Swing}

class PlayerBattleView(player: Player, mainFrame: GUIEventsHandler) extends BattleView(player) {

  player match
    case First =>
      centerLabel.text = first_player_turn
      for(i <- 0 until PLAYER_HOLES) {
        secondPlayerButtonArray(i).enabled = false

        firstPlayerButtonArray(i).action = Action(firstPlayerButtonArray(i).text){
          handleButtonClick(First, i)
        }

      }
    case Second =>
      centerLabel.text = second_player_turn
      for(i <- 0 until PLAYER_HOLES) {
        firstPlayerButtonArray(i).enabled = false

        secondPlayerButtonArray(i).action = Action(firstPlayerButtonArray(i).text){
          handleButtonClick(Second, i)
        }
      }
      /* implementation for server view
    case _ =>
      for(i <- 0 until PLAYER_HOLES) {
        firstPlayerButtonArray(i).enabled = false
        secondPlayerButtonArray(i).enabled = false
      }*/


  val macalasPanel = new BorderPanel{

    player match
      case First =>
        add(secondMacala, BorderPanel.Position.West)
        add(centerLabel, (BorderPanel.Position.Center))
        add(firstMacala, BorderPanel.Position.East)
      case Second =>
        add(firstMacala, BorderPanel.Position.West)
        add(centerLabel, (BorderPanel.Position.Center))
        add(secondMacala, BorderPanel.Position.East)
        //server view
        /*
      case _ =>
        add(secondMacala, BorderPanel.Position.West)
        add(centerLabel, (BorderPanel.Position.Center))
        add(firstMacala, BorderPanel.Position.East)
        */
  }

  macalasPanel.background = opaqueColor
  macalasPanel.opaque = opaqueSetting

/*
  val infoPanel = new FlowPanel{
      infoLabel
  }*/

  private val playerPerspective = new GridPanel(3, 1){

    player match
      case First =>
        contents += secondPlayerButtons
        contents += macalasPanel
        contents += firstPlayerButtons
      case Second =>
        contents += firstPlayerButtons
        contents += macalasPanel
        contents += secondPlayerButtons
        //server view
        /*
      case _ =>
        contents += secondPlayerButtons
        contents += macalasPanel
        contents += firstPlayerButtons
        */
  }

  playerPerspective.background = opaqueColor
  playerPerspective.opaque = opaqueSetting

  private val battleView = BackgroundGradPanel(gradType = HORIZONTAL)(
    Swing.VStrut(2*PREFERED_HEIGHT/3),
    playerPerspective
  )

  private def handleButtonClick(player: Player, buttonIndex: Int): Unit =
    mainFrame.onPlayerClick(player, buttonIndex)
  
  def updateButtonsView(gameBoard: GameBoard): Unit =
    firstMacala.repaint()
    secondMacala.repaint()
    centerLabel.repaint()
    player match {
      case First =>
        for (i <- 0 until PLAYER_HOLES+1){
          firstPlayerButtonArray(i).text = "%2d".format(gameBoard.getFirstPlayerPit(i))
          firstPlayerButtonArray(i).repaint()
          secondPlayerButtonArray(i).text = "%2d".format(gameBoard.getSecondPlayerPit(i))
          secondPlayerButtonArray(i).repaint()
        }
      case Second =>
        for (i <- 0 until PLAYER_HOLES+1){
          firstPlayerButtonArray(i).text = "%2d".format(gameBoard.getFirstPlayerPit(i))
          firstPlayerButtonArray(i).repaint()
          secondPlayerButtonArray(i).text = "%2d".format(gameBoard.getSecondPlayerPit(i))
          secondPlayerButtonArray(i).repaint()
        }
    }


  def getView(): BackgroundGradPanel =
    battleView


}
