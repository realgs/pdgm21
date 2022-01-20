import scala.io.StdIn.readLine

class Server {

  var board: Board = _
  var currentPlayer: Int = _
  var p1: Player = _
  var p2: Player = _
  var timeForMove: Int = 30
  var read: String = _

  def startGame(): Unit = {
    board = new Board
    board.printScheme()
    println("Choose game type:\n1.Player vs Player\n2.Player vs Computer\n3.Computer vs Computer")
    read = readLine()
    read match {
      case "1" =>
        println("Player 1 name:")
        read = readLine()
        p1 = new Human(0, read)
        println("Player 2 name:")
        read = readLine()
        p2 = new Human(1, read)
        HvH()
      case "2" =>
        println("Player name:")
        read = readLine()
        p1 = new Human(0, read)
        p2 = new Engine(1, "Computer")
        HvC()
      case "3" =>
        p1 = new Engine(0, "Computer 1")
        p2 = new Engine(1, "Computer 2")
        CvC()
    }
  }

  def HvH(): Unit = {
    board.printBoard()
    if currentPlayer == 0 then {
      println(s"${p1.name} move")
      read = readLine()
      var next = board.move(read.toInt, 0)
      if board.isThisTheEnd(0) then {
        board.result() match {
          case 0 => println(s"${p1.name} wins")
          case 1 => println(s"${p2.name} wins")
          case -1 => println("Draw")
        }
      }
      else {
        currentPlayer = next
        HvH()
      }
    }
    else {
      println(s"${p2.name} move")
      read = readLine()
      var next = board.move(read.toInt, 1)
      if board.isThisTheEnd(1) then {
        board.result() match {
          case 0 => println(s"${p1.name} wins")
          case 1 => println(s"${p2.name} wins")
          case -1 => println("Draw")
        }
      }
      else {
        currentPlayer = next
        HvH()
      }
    }
  }

  def HvC(): Unit = {
    board.printBoard()
    if currentPlayer == 0 then {
      println(s"${p1.name} move")
      read = readLine()
      var next = board.move(read.toInt, 0)
      if board.isThisTheEnd(currentPlayer) then {
        board.result() match {
          case 0 => println(s"${p1.name} wins")
          case 1 => println(s"${p2.name} wins")
          case -1 => println("Draw")
        }
      }
      else {
        currentPlayer = next
        HvC()
      }
    }
    else {
      val holeIndex = p2.asInstanceOf[Engine].highestScore(board)
      println(s"${p2.name} move:" + holeIndex)
      val next = board.move(holeIndex, 1)
      if board.isThisTheEnd(currentPlayer) then {
        board.result() match {
          case 0 => println(s"${p1.name} wins")
          case 1 => println(s"${p2.name} wins")
          case -1 => println("Draw")
        }
      }
      else {
        currentPlayer = next
        HvC()
      }
    }
  }

  def CvC(): Unit = {
    board.printBoard()
    if currentPlayer == 0 then {
      val holeIndex = p1.asInstanceOf[Engine].highestScore(board)
      println(s"${p1.name} move:" + holeIndex)
      val next = board.move(holeIndex, 0)
      if board.isThisTheEnd(currentPlayer) then {
        board.result() match {
          case 0 => println(s"${p1.name} wins")
          case 1 => println(s"${p2.name} wins")
          case -1 => println("Draw")
        }
      }
      else {
        currentPlayer = next
        CvC()
      }
    }
    else {
      val holeIndex = p2.asInstanceOf[Engine].highestScore(board)
      println(s"${p2.name} move:" + holeIndex)
      val next = board.move(holeIndex, 1)
      if board.isThisTheEnd(currentPlayer) then {
        board.result() match {
          case 0 => println(s"${p1.name} wins")
          case 1 => println(s"${p2.name} wins")
          case -1 => println("Draw")
        }
      }
      else {
        currentPlayer = next
        CvC()
      }
    }
  }

}
