class Computer (val board : Board) extends Player {
  def move(): Int ={
    val number = bestMove(board)
    val numberprint = number - 7
    println(s"Komputer - numer pola : $numberprint" )
    number

  }


  def bestMove(board : Board): Int ={
    var bestChoice = -1
    var highestScore = Int.MinValue

    for (i <- 0 to 12){
      if (board.checkChoice(i)){
        val newBoard = board.cloneBoard()
        newBoard.moveSeedsFrom(i)
        newBoard.nextPlayer()
        val advantage = checkPossibleMoves(newBoard, 4, board.findActivePlayer() == newBoard.findActivePlayer(), board.findActivePlayer())

        if(advantage > highestScore || bestChoice == -1){
          highestScore = advantage
          bestChoice = i
        }
      }
    }
    return bestChoice
  }


  def checkPossibleMoves(board : Board, depth : Int, better : Boolean, player : Boolean): Int = {
    var currentAdvantage = Int.MinValue

    if (depth == 0) return board.calculate(board.findActivePlayer())
    if (!board.isNextMovePossibe()) {
      board.collectAllSeeds()
      if (board.calculate(player) > 0) {
        return Int.MaxValue
      }
      else return Int.MinValue
    }
    if (better) {
      for (i <- 0 to 12) {
        if (board.checkChoice(i)) {
          val newboard = board.cloneBoard()
          newboard.moveSeedsFrom(i)
          newboard.nextPlayer()
          val advantage = checkPossibleMoves(newboard, depth - 1, player == newboard.findActivePlayer(), player)
          currentAdvantage = currentAdvantage.max(advantage)
        }
      }
    }
    else {
      currentAdvantage = Int.MaxValue
      for (i <- 0 to 12) {
        if (board.checkChoice(i)) {
          val newboard = board.cloneBoard()
          newboard.moveSeedsFrom(i)
          newboard.nextPlayer()
          val advantage = checkPossibleMoves(newboard, depth - 1, player == newboard.findActivePlayer(), player)
          currentAdvantage = currentAdvantage.min(advantage)

        }
      }
    }
    currentAdvantage
  }
}

