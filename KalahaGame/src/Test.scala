import scala.io.StdIn.readInt

//import Board
object Test {

  def main(args: Array[String]): Unit = {
    var board = new Board()
    printStartInfo()
    val (v1, v2): (Int, Int) = selectMode()

    if v1 == 0 then
      var game = new KalahaGame(new Computer(1, board), new Computer(2, board), board)
      game.play()
    else if v2 == 1 then
      var game = new KalahaGame(new HumanPlayer(1), new Computer(2, board), board)
      game.play()
    else if v2 == 2 then
      var game = new KalahaGame(new HumanPlayer(1), new HumanPlayer(2), board)
      game.play()

      }

    def printStartInfo(): Unit = {
      println("Welcome to Kalaha game!")
      println("Choose game mode:")
      println("0 - simulation computer - computer")
      println("1 - human - computer")
    }

    def selectMode(): (Int, Int) = {
      var correctArg = false
      var choice = -1
      while (!correctArg) {
        try {
          choice = readInt()
          if (choice != 1 && choice != 0) {
            println("Your choice must be only 0 or 1")
          } else {
            correctArg = true
          }
        } catch {
          case e => println("Ivalid input format, must be an integer number")
        }
      }

      choice match {
        case 0 => {
          (0, 0)
        }
        case 1 => {
          correctArg = false
          var playerChoice = -1
          while (!correctArg) {
            println("Choose your player number:")
            try {
              playerChoice = readInt()
              if (playerChoice != 1 && playerChoice != 2) {
                println("Your choice must be only 1 or 2")
              } else {
                correctArg = true
              }
            } catch {
              case e => println("Ivalid input format, must be an integer number")
            }
          }
          playerChoice match {
            case 1 => {
              (1, 1)
            }
            case 2 => {
              (1, 2)
            }
          }
        }
      }
    }
}
