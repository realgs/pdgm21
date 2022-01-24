package players

import board.Board
import players.PlayerID.PlayerID
import game_system.GameParameters.HOUSE_NR

import scala.util.Random

class AIPlayer(id: PlayerID) extends Player(id):

  private val TREE_DEPTH = 4

  def makeMove(): Int =
    makeMoveRandom()

  def makeMoveRandom(): Int =
    Random.between(1, HOUSE_NR)

  def makeMove(board: Board): Int =
    println("Player " + id + " (Computer move)")
    var bestMove = makeMoveRandom() //default turn is random turn
    var maxEval = Integer.MIN_VALUE

    for (move <- 1 to HOUSE_NR) {
      if !board.checkIfEmptyHouse(move - 1, id) then
        val state = board.clone()
        state.executeMove(move, id)

        val eval = minimax(state, TREE_DEPTH, false, id)
        if eval > maxEval then
          maxEval = eval
          bestMove = move
        else ()
      else ()
    }
    bestMove

  private def minimax(state: Board, depth: Int, isMaximizing: Boolean, prevPlayerID: PlayerID): Int =
    var currPlayerID = prevPlayerID

    if prevPlayerID == PlayerID.first then
      currPlayerID = PlayerID.second
    else
      currPlayerID = PlayerID.first

    //score this state
    if depth == 0 || !state.checkIfMoreMovesPossible(currPlayerID) then
      return state.getScore(prevPlayerID) - state.getScore(currPlayerID)

    //Find max evaluation out of children
    if isMaximizing then
      var maxEval = Integer.MIN_VALUE

      for (move <- 1 to HOUSE_NR) {
        if !state.checkIfEmptyHouse(move - 1, id) then //avoid empty houses
          val newState = state.clone()
          newState.executeMove(move, currPlayerID)

          val eval = minimax(newState, depth - 1, false, currPlayerID)
          if (eval > maxEval) maxEval = eval
      }
      maxEval
    //Find min evaluation out of children
    else
      var minEval = Integer.MAX_VALUE

      for (move <- 1 to HOUSE_NR) {
        if !state.checkIfEmptyHouse(move - 1, id) then //avoid empty houses
          val newState = state.clone()
          newState.executeMove(move, currPlayerID)

          val eval = minimax(newState, depth - 1, true, currPlayerID)
          if (eval < minEval) minEval = eval
      }
      minEval
