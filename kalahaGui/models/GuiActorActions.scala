package kalaha.models

import kalaha.models.PlayerEnum.Player

object GuiActorActions {
  case class Refresh(gameBoard: GameBoard, player: Player)
  case class PrepareForBtnClick(player: Player)
  case class SendBtnInfoBack(player: Player, pit: Int)

  case class ShowDialogWon(myScore: Int, opponentScore: Int, who: String)
  case class ShowDialogComputerWon(myScore: Int, opponentScore: Int, who: String)
  case class ShowDialogDraw(score: Int)

  case class ShowDialogAbout()
}
