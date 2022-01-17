package kalaha.models

import kalaha.models.PlayerEnum.{First, Player, Second}
import kalaha.utils.Constants.PLAYER_HOLES
import scala.util.control.Breaks.{break, breakable}

class DecisionTree(private val player: Player) {

  private var winnerPlayer: Player = _
  private var loserPlayer: Player = _

  player match
    case First =>
      winnerPlayer = First
      loserPlayer = Second
    case Second =>
      winnerPlayer = Second
      loserPlayer = First

  def minimax(gameBoard: GameBoard, depth: Int, alpha: Int, beta: Int, isMaximizingPlayer: Boolean): Int =


    if gameBoard.isEndOfGame() || depth == 0 then
      player match
        case First => gameBoard.getFirstPlayerScore()
        case Second => gameBoard.getSecondPlayerScore()
    else
      if isMaximizingPlayer then
        var localAlpha = alpha
        var maxScore = Integer.MIN_VALUE

        breakable{
          for (hole <- 0 until PLAYER_HOLES) {
            if gameBoard.isMoveValid(hole) then
              val copyBoard = gameBoard.clone()
              copyBoard.changeToSimulationMode()

              copyBoard.moveStones(hole)
              val score = minimax(
                copyBoard, depth-1, localAlpha, beta, copyBoard.getTurn() == player
              )

              maxScore = math.max(maxScore, score)
              localAlpha = math.max(localAlpha, score)

              if beta <= localAlpha then
                break
          }
        }

        maxScore
      else
        var localBeta = beta
        var minScore = Integer.MAX_VALUE

        breakable{
          for (hole <- 0 until PLAYER_HOLES){
            if gameBoard.isMoveValid(hole) then
              val copyBoard = gameBoard.clone()
              copyBoard.changeToSimulationMode()
              
              copyBoard.moveStones(hole)

              val score = minimax(
                copyBoard, depth-1, alpha, localBeta, copyBoard.getTurn() == player
              )

              minScore = math.min(minScore, score)
              localBeta = math.min(localBeta, score)

              if localBeta < score then
                break
          }
        }

        minScore
}
