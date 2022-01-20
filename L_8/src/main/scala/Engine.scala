import scala.util.Random

class Engine(playerNr: Int, name: String) extends Player(playerNr, name) {

  def highestScore(board: Board): Int = {
    var score: Int = 0
    var bestChoiceIndex = 0
    for (i <- 0 to board.sideSize - 1) {
      var board_clone = board.cloneBoard()
      board_clone.move(i, playerNr)
      var currentScore = if playerNr == 0 then board_clone.board(board_clone.base0Index) else board_clone.board(board_clone.base1Index)
      if currentScore >= score then
        score = currentScore
        bestChoiceIndex = i
    }
    return bestChoiceIndex
  }

  def isNextMovePossible(board: Board): (Boolean, Int) = {
    for(i <- board.base0Index - 1 to 0 by -1) {
      if board.board(i) + i == board.base0Index then return(true, i)
    }
    return (false, -1)
  }

  def scoreOfEnemy(board: Board): Int ={
    val player = Player(board)
    val difference: Array[Int] = Array.ofDim(player._4 - player._3)
    for(i <- player._3 to player._4 - 1) {
      if board.board(i) == 0 then difference(i - player._3) = Int.MinValue
      else {
        var board_clone = board.cloneBoard()
        val same = if playerNr == 0 then board_clone.move(i - player._3, 1) else board_clone.move(i - player._3, 0)
        difference(i - player._3) = board_clone.board(player._4) - board_clone.board(player._2)
        if same == (playerNr + 1) % 2 then difference(i - player._3) = difference(i - player._3) + 1
      }
    }
    return difference.max
  }

  def bestMove(board: Board): Int = {
    val player = Player(board)
    val rnd = new Random
    val difference: Array[Int] = Array.ofDim(player._2 - player._1)
    for(i <- player._1 to player._2 - 1) {
      if board.board(i) == 0 then difference(i - player._1) = Int.MaxValue
      else {
        var board_clone: Board = board.cloneBoard()
        board_clone.move(i - player._1, playerNr)
        difference(i - player._1) = scoreOfEnemy(board_clone)
      }
    }
    var min = difference.min
    var chosen = List[Int]()
    for(i <- 0 to difference.length - 1){
      if difference(i) == min then chosen = i :: chosen
    }
    return chosen(rnd.nextInt(chosen.length))
  }

  def highestScore2(board: Board): Int = {
    val (nextMovePossible, holeIndex) = isNextMovePossible(board)
    if nextMovePossible then return holeIndex
    else return bestMove(board)
  }

  def Player(board: Board): (Int, Int, Int, Int) = {
    val base0Index: Int = board.base0Index
    val base1Index: Int = board.base1Index
    if playerNr == 0 then return(0, base0Index, base0Index + 1, base1Index)
    else return (base0Index + 1, base1Index, 0, base1Index)
  }
}
