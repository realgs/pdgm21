import com.sun.source.tree.WhileLoopTree

class Board:
  var board = Array.ofDim[Int](14)
  var playerAName = "Player A"
  var playerBName = "Player B"

  //constructor
  def this(board: Array[Int], playerAName: String, playerBName: String) = {
    this()
    this.board = board
    this.playerAName = playerAName
    this.playerBName = playerBName
  }

  def getPlayerABase() =
    board(6)

  def getPlayerBBase() =
    board(13)

  def boardInit(clientA: String = "Gracz A", clientB: String = "Gracz B") =
    board = Array.ofDim[Int](14)
    playerAName = clientA
    playerBName = clientB


    for (i <- 0 to 5) {
      board(i) = 6 //ilość kamieni w otworach
    }
    for (i <- 7 to 12) {
      board(i) = 6 //ilość kamieni w otworach
    }

  def moveStones(number: Int, player: Int): Int = //0 - error, 1 - success, 2 - end in base (extra move)
    if (player == 1)
      if (number >= 7 && number <= 12 && board(number) > 0)
        val end = placeStones(board(number), number + 1, player)
        board(number) = 0
        end
      else
        0
    else if (number >= 0 && number <= 5 && board(number) > 0)
      val end = placeStones(board(number), number + 1, player)
      board(number) = 0
      end
    else
      0


  def placeStones(stones: Int, number: Int, player: Int): Int = //0 - error, 1 - success, 2 - end in base (extra move)
    var stonesLeft = stones
    if number >= 0 && number <= 5 then
      board(number) += 1
      stonesLeft -= 1
      if stonesLeft == 0 && player == 0 && board(number) == 1 then
        board(6) += board(12 - number)
        board(12 - number) = 0
    else if number >= 7 && number <= 12 then
      board(number) += 1
      stonesLeft -= 1
      if stonesLeft == 0 && player == 1 && board(number) == 1 then
        board(13) += board(12 - number)
        board(12 - number) = 0
    else if player == 0 && number == 6 then
      board(number) += 1
      stonesLeft -= 1
      if stonesLeft == 0 then
        return 2
    else if player == 1 && number == 13 then
      board(number) += 1
      stonesLeft -= 1
      if stonesLeft == 0 then
        return 2
    if stonesLeft <= 0 then
      return 1
    if number > 13 then
      placeStones(stonesLeft, 0, player)
    else
      placeStones(stonesLeft, number + 1, player)


  def validMove(number: Int, player: Int): Boolean =
    if (player == 1)
      if (number >= 7 && number <= 12 && board(number) > 0)
        true
      else
        false
    else if (number >= 0 && number <= 5 && board(number) > 0)
      true
    else
      false

  def checkEmpty(): Boolean =
    for (i <- 0 to 5)
      if (board(i) > 0)
        return false
    for (i <- 7 to 12)
      if (board(i) > 0)
        return false
    true


  def printBoard(playerView: Int = 0): Unit =
    if playerView == 0 then
      print("\tGracz " + playerBName + "\n\t 6  \t 5  \t 4  \t 3  \t 2  \t 1  \n\t")
      for (i <- 12 to 7 by -1)
        print("[" + board(i) + "] \t")
      print("\n\n{" + board(13) + "}\t\t\t\t\t\t\t\t\t\t\t\t{" + board(6) + "}\n\n\t")
      for (i <- 0 to 5)
        print("[" + board(i) + "] \t")
      print("\n\t 1  \t 2  \t 3  \t 4  \t 5  \t 6\n\tGracz " + playerAName + "\n\n")
    else
      print("\tGracz " + playerAName + "\n\t 6  \t 5  \t 4  \t 3  \t 2  \t 1  \n\t")
      for (i <- 5 to 0 by -1)
        print("[" + board(i) + "] \t")
      print("\n\n{" + board(6) + "}\t\t\t\t\t\t\t\t\t\t\t\t{" + board(13) + "}\n\n\t")
      for (i <- 7 to 12)
        print("[" + board(i) + "] \t")
      print("\n\t 1  \t 2  \t 3  \t 4  \t 5  \t 6\n\tGracz " + playerBName + "\n\n")

  def toString(playerView: Int = 0): String = {
    var result = ""
    if playerView == 0 then
      result += "\tGracz " + playerBName + "\n\t 6  \t 5  \t 4  \t 3  \t 2  \t 1  \n\t"
      for (i <- 12 to 7 by -1)
        result += "[" + board(i) + "] \t"
      result += "\n\n{" + board(13) + "}\t\t\t\t\t\t\t\t\t\t\t\t{" + board(6) + "}\n\n\t"
      for (i <- 0 to 5)
        result += "[" + board(i) + "] \t"
      result += "\n\t 1  \t 2  \t 3  \t 4  \t 5  \t 6\n\tGracz " + playerAName + "\n\n"
    else
      result += "\tGracz " + playerAName + "\n\t 6  \t 5  \t 4  \t 3  \t 2  \t 1  \n\t"
      for (i <- 5 to 0 by -1)
        result += "[" + board(i) + "] \t"
      result += "\n\n{" + board(6) + "}\t\t\t\t\t\t\t\t\t\t\t\t{" + board(13) + "}\n\n\t"
      for (i <- 7 to 12)
        result += "[" + board(i) + "] \t"
      result += "\n\t 1  \t 2  \t 3  \t 4  \t 5  \t 6\n\tGracz " + playerBName + "\n\n"
    result
  }

  //copy
  def copy() =
    new Board(board.clone(), playerAName, playerBName)

  def objectToString() =
    board.mkString(" ") + "," + playerAName + "," + playerBName

  def stringToObject(string: String) =
    string.split(",") match
      case Array(board, playerAName, playerBName) =>
        new Board(board.split(" ").map(_.toInt), playerAName, playerBName)

  def bestSolution(number: Int) = {
    var bestIndex = 0
    var bestScore = 0
    if number == 0 then
      for (i <- 0 to 5)
        var copy = this.copy()
        copy.moveStones(i, 0)
        if copy.getPlayerABase() > bestScore then
          bestIndex = i
          bestScore = copy.getPlayerABase()
        else
          ()
      val copy = this.copy()
      while !copy.validMove(bestIndex, 0) do
        bestIndex += 1
    else
      bestIndex = 7
      for (i <- 7 to 12)
        val copy = this.copy()
        copy.moveStones(i, 1)
        if copy.getPlayerBBase() > bestScore then
          bestIndex = i
          bestScore = copy.getPlayerBBase()
      val copy = this.copy()
      while !copy.validMove(bestIndex, 1) do
        bestIndex += 1
      bestIndex -= 7
    bestIndex + 1
  }


  def bestSolutionAI(player: Int) =
    val startTime = System.currentTimeMillis()
    var bestSolution = 0
    var iterator = 0
    var output = 0
    while (output>=0&&startTime+25000>System.currentTimeMillis()) do
      println("Iteration: " + iterator)

      output = miniMax(this.copy(),iterator,player, player, startTime)
      if output>=0 then
        if output>=6 then
          bestSolution = output - 7
        else
          bestSolution = output
        println("Output: "+bestSolution)
        iterator += 1
      else
        println("TIMEOUT")
    bestSolution+1




  def miniMax(board: Board, depth: Int, player: Int, startPlayer: Int, startTime: Long): Int =
    if startTime + 25000<System.currentTimeMillis() then
      return -1
    if depth == 0 then
      return board.bestSolution(player)+player*7
    else
      if player == startPlayer then
        var index = 0
        var score = 0
        var bestScore = Integer.MIN_VALUE
        var bestIndex = 0
        for (i <- 0 to 5)
          if board.validMove(i+player*7, player) then
            val copy = board.copy()
            var score = 0
            if copy.moveStones(i+player*7, player) == 2 then
              index = miniMax(copy, depth - 1, player, startPlayer, startTime)
            else
              index = miniMax(copy, depth - 1, 1 - player, startPlayer, startTime)
            score = board.copy().moveStones(index, player)
            if score > bestScore then
              bestScore = score
              bestIndex = index
        return bestIndex
      else
        var index = 0
        var score = 0
        var worstScore = Integer.MAX_VALUE
        var worstIndex = 0
        for (i <- 0 to 5)
          if board.validMove(i+player*7, player) then
            val copy = board.copy()
            if copy.moveStones(i+player*7, player) == 2 then
              index = miniMax(copy, depth - 1, player, startPlayer, startTime)
            else
              index = miniMax(copy, depth - 1, 1 - player, startPlayer, startTime)
            score = board.copy().moveStones(index, player)
            if score < worstScore then
              worstScore = score
              worstIndex = index
        return worstIndex




  def clearHoles() =
    for (i <- 0 to 5)
      if (board(i) > 0)
        board(6) += board(i)
        board(i) = 0
    for (i <- 7 to 12)
      if (board(i) > 0)
        board(13) += board(i)
        board(i) = 0


  def checkEnd(player: Int): Boolean =
    if player == 0 then
      for (i <- 0 to 5)
        if (board(i) > 0)
          return false
        else
          ()
    else if player == 1 then
      for (i <- 7 to 12)
        if (board(i) > 0)
          return false
        else
          ()
    clearHoles()
    true