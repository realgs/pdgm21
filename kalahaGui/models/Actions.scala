package kalaha.models

object Actions {
  case class Move(gameBoard: GameBoard)

  case class YouWon(firstScore: Int, secondScore: Int)
  case class Draw(score: Int)
}
