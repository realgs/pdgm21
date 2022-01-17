package kalaha.actors

import akka.actor.{Actor, ActorRef}
import kalaha.models.GuiActorActions.*
import kalaha.models.PlayerEnum.Player
import kalaha.models.{GUIEventsHandler, GameBoard}

class GUIUpdateActor(private val gui: GUIEventsHandler) extends Actor{

  private val whoSend: Array[ActorRef] = new Array[ActorRef](2)

  def receive = {
    case Refresh(gameBoard: GameBoard, player: Player) =>
      whoSend(player.id) = sender()
      gui.switchToView(player)
      gui.refreshButtons(gameBoard, player)

    case PrepareForBtnClick(player) =>
      whoSend(player.id) = sender()

    case SendBtnInfoBack(player, pit) =>
      sendBack(player, pit)

    case ShowDialogWon(myScore, opponentScore, who) =>
      gui.showDialogWon(myScore, opponentScore, who)

    case ShowDialogDraw(score) =>
      gui.showDialogDraw(score)

    case ShowDialogComputerWon(myScore, opponentScore, who) =>
      gui.showDialogWon(myScore, opponentScore, who)

    case ShowDialogAbout =>
      gui.showDialogAbout()

  }
  
  private def sendBack(player: Player, pit: Int): Unit =
    if whoSend(player.id) != null then
      whoSend(player.id) ! pit
      whoSend(player.id) = null

}
