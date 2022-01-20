class Computer extends Player {
  override def makeMove(board: Board): Int = {
    Thread.sleep(1000)
    var firstPlayer = board.isMoveFirstPlayer

    if (board.isMoveFirstPlayer) {
      findBestPossision(makeResultArray(board))
    } else {
      findBestPossision(makeResultArray(board)) + 6
    }
  }

  def findBestPossision(result: Array[Int]): Int = {
    var bestResultPosision = 0
    var bestResult = result(0)

    for (i <- 0 to 5) {
      if (result(i) > bestResult) then {
        bestResult = result(i)
        bestResultPosision = i
      }
    }
    bestResultPosision + 1
  }

  def makeResultArray(board: Board): Array[Int] = {
    val result: Array[Int] = new Array[Int](6)
    for (i <- 0 to 5) {
      if (board.isMoveFirstPlayer) {
        result(i) = countBestResult(i + 1, board.clone())
      } else {
        result(i) = countBestResult(i + 7, board.clone())
      }

    }
    result
  }

  def countBestResult(possition: Int, board: Board): Int = {
    if (goodNumber(possition, board)) {
      board.MakeMove(possition)
      var result = board.getSourse._2 - board.getSourse._1
      if (board.isMoveFirstPlayer) then result = result * (-1)
      if (board.isCanMoreMove) then result + countBestResult(findBestPossision(makeResultArray(board)), board)
      result
    } else {
      return Int.MinValue
    }
  }

  def goodNumber(number: Int, board: Board): Boolean = {
    if (board.isMoveFirstPlayer) {
      number >= 1 && number <= 6 && board.board(number - 1) != 0
    }
    else {
      number >= 7 && number <= 12 && board.board(number) != 0
    }
  }

}
