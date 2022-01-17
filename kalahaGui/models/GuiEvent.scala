package kalaha.models

import kalaha.models.GuiActorActions.ShowDialogComputerWon
import kalaha.models.PlayerEnum.Player

abstract class GuiEvent

trait GUIEventsHandler {

  def switchToView(player: Player): Unit
  def onPlayerClick(player: Player, buttonIndex: Int): Unit
  def refreshButtons(gameBoard: GameBoard, player: Player): Unit

  def showDialogWon(myScore: Int, opponentScore: Int, who: String): Unit
  def showDialogDraw(score: Int): Unit

  def showDialogAbout(): Unit
}


