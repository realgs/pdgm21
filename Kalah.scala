class Kalah(val player:String = "player1", val oponent: String = "player2") {
  private var game: Array[Node] = createBoard();

  def getBoard(): Array[Node] = {
    game
  }

  def checkIfEmpty(index:Int): Boolean = {
    if game(index).getStones() == 0 then true
    else false
  }

  def createBoard(): Array[Node] = {
    var board: Array[Node] = new Array[Node](14)
    board(6) = new Node(player, 0, true)
    board(13) = new Node(oponent, 0, true)

    for(i <- 0 to 5) {
      board(i) = new Node(player, 6, false)
    }

    for(i <- 7 to 12) {
      board(i) = new Node(oponent, 6, false)
    }



    for(i<-0 to 12) {
      board(i).setNext(i+1)
    }
    board(13).setNext(0)

    board(0).setOpposite(12)
    board(12).setOpposite(0)
    board(1).setOpposite(11)
    board(11).setOpposite(1)
    board(2).setOpposite(10)
    board(10).setOpposite(2)
    board(3).setOpposite(9)
    board(9).setOpposite(3)
    board(4).setOpposite(8)
    board(8).setOpposite(4)
    board(5).setOpposite(7)
    board(7).setOpposite(5)



    board
  }

  def makeMove(index:Int, name:String): String = {
    var stonesInHole = game(index).getStones()
    var move = name
    var holeNumber = index
    game(index).setStones(0)
    while (stonesInHole > 0) {
      holeNumber += 1
      if holeNumber == 14 then holeNumber = 0
      if (stonesInHole == 1 && name == whoseSide(game(holeNumber).nextIndex) && game(holeNumber).getStones() == 0) {

        if (name == "player1") {
          var oponentIndex = game(holeNumber).oppositeIndex
          game(6).setStones(game(6).getStones() + game(oponentIndex).getStones() + 1)
          game(holeNumber).setStones(0)
          game(oponentIndex).setStones(0)
          stonesInHole -= 1
        } else if (name == "player2") {
          var oponentIndex = game(holeNumber).oppositeIndex
          game(13).setStones(game(13).getStones() + game(oponentIndex).getStones() + 1)
          game(oponentIndex).setStones(0)
          game(holeNumber).setStones(0)
          stonesInHole -= 1
        }


      } else {
        if (name == "player1" && holeNumber == 13 || name == "player2" && holeNumber == 6) {

        } else if (name == "player1" && game(holeNumber).getIfMyHole()) {
          game(holeNumber).setStones(game(holeNumber).getStones() + 1)
          stonesInHole -= 1
        } else if (name == "player2" && game(holeNumber).getIfMyHole()) {
          game(holeNumber).setStones(game(holeNumber).getStones() + 1)
          stonesInHole -= 1
        } else {
          game(holeNumber).setStones(game(holeNumber).getStones() + 1)
          stonesInHole -= 1
        }

        if (stonesInHole == 1 && name == "player1" && game(holeNumber).nextIndex == 6) {
          move = oponent
        } else if (stonesInHole == 1 && name == "player2" && game(holeNumber).nextIndex == 13) {
          move = player
        }


      }


    }

    whoseMove(move)
  }

  def whoseSide(nextId: Int):String = {
    nextId match {
      case 0 => "player1"
      case 1 => "player1"
      case 2 => "player1"
      case 3 => "player1"
      case 4 => "player1"
      case 5 => "player1"
      case 6 => "player1"
      case 7 => "player2"
      case 8 => "player2"
      case 9 => "player2"
      case 10 => "player2"
      case 11 => "player2"
      case 12 => "player2"
      case 13 => "player2"
      case _ => ""
    }
  }



  def checkIfEnd(name:String): Boolean = {
    var sum = 0;
    var stones = 0
    var checkPlayer1 = false
    var checkPlayer2 = false

    if (name == "player1") {
      for (i <- 0 to 5) {
        sum += game(i).getStones()
      }
      if (sum == 0) {
        checkPlayer1 = true
        for (i <- 7 to 12) {
          //stones += game(i).getStones()
          game(i).setStones(0)
        }
        game(13).setStones(game(13).getStones() + stones)

      }

    } else if (name == "player2") {
      for (i <- 7 to 12) {
        sum += game(i).getStones()
      }
      if (sum == 0) {
        checkPlayer2 = true
        for (i <- 0 to 5) {
          //stones += game(i).getStones()
          game(i).setStones(0)
        }
        game(6).setStones(game(6).getStones() + stones)
      }
    }

    checkPlayer1 || checkPlayer2
  }

  def whoseMove(name:String): String = {
    if name == "player1" then oponent
    else player
  }

  def printBoard() = {
    var i = 12
    print(" \t ")
    while (i >= 7) {
      print(game(i).getStones() + "\t ")
      i = i - 1
    }
    println()
    print(game(13).getStones() + "\t\t\t\t\t\t\t\t" + game(6).getStones())
    println()
    print(" \t ")
    for (i <- 0 to 5) {
      print(game(i).getStones() + "\t ")
    }
    println()

  }

  def whoWins():Unit = {
    if (game(13).getStones() > game(6).getStones()) then
      println("WINNER: " + oponent + " SCORE: " +game(13).getStones())
      println("OPONENT: " + player + " SCORE: " +game(6).getStones())
    else if (game(13).getStones() < game(6).getStones()) then
      println("WINNER: " + player + " SCORE: " +game(6).getStones())
      println("OPONENT: " + oponent + " SCORE: " +game(13).getStones())
    else println("DRAW:\n" + oponent + ": " + game(13).getStones() +"\n" + player + ": " + game(6).getStones())

  }

  def copyBoard(): Kalah = {
    var kalahCopy: Kalah = new Kalah(player, oponent)
    for (i <- 0 to game.length - 1) {
      kalahCopy.getBoard()(i) = new Node(game(i).getPlayer(), game(i).getStones(), game(i).getIfMyHole())
    }
    kalahCopy
  }

 



}
