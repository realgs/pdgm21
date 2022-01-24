import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import java.util.concurrent.{TimeUnit, TimeoutException}
import scala.concurrent.duration.Duration

class Board(private var player1 : Player = Player(), private var player2 : Player = Player()) {
  private var playerTurn = 1
  private var currentPlayer : Player = player1

  def startGame() : Unit = {
    currentPlayer = player1

    clearScreen()

    while(!isGameFinished()) {
      printBoard()

      if playerTurn == 1 then {
        println("Player1's MOVE (30 seconds to move)")
      }
      else {
        println("Player2's MOVE (30 seconds to move")
      }
      print("Choose house: ")


      val choiceFuture = Future {
        currentPlayer.makeMove()
      }

      try {
        val choice = Await.result(choiceFuture, Duration(10, TimeUnit.SECONDS))
        if choice == Int.MinValue then ()
        else makeMove(choice)
        clearScreen()
      } catch {
        case _: TimeoutException => {
          println("\nTime is Over!")
          println("Your turn is skipped")
          Thread.sleep(1000)
          changePlayer()
        }
      }
    }

  }

  // -1 means double move
  def makeMove(choice : Int) : Int = {
    var left = currentPlayer.takeAllFrom(choice - 1)
    if (left._1 == -2) {
      changePlayer()
      val houseNumber = 5 - left._2
      val enemyHouseSeeds = currentPlayer.clearHouse(houseNumber)
      changePlayer()
      currentPlayer.clearHouse(left._2)
      currentPlayer.addToBase(enemyHouseSeeds + 1)
      changePlayer()
      0
    }
    else if left._1 == -1 then { -1 }
    else {
      if left._1 == 0 then changePlayer()
      else {
        var counter = 0
        while (left._1 > 0) {
          counter = counter + 1
          changePlayer()
          left = currentPlayer.takeFromRivalMove(left._1)
        }
        if counter % 2 == 0 then changePlayer()
      }
      0
    }
  }

  def isGameFinished() : Boolean = {
    changePlayer()
    var toReturn = currentPlayer.canMakeMove()
    if !toReturn then return true
    changePlayer()
    toReturn = currentPlayer.canMakeMove()
    if !toReturn then return true
    return false
  }

  def gameEnd() : Unit = {
    printBoard()
    changePlayer()
    currentPlayer.gameEnd()
    changePlayer()
    currentPlayer.gameEnd()
    println("\nGAME FINISHED !!!")
    if player1.getBase > player2.getBase then println("PLAYER 1 WON")
    else if player1.getBase < player2.getBase then println("PLAYER 2 WON")
    else println("IT A DRAW")
    println("RESULTS -> " + player1.getBase + " - " + player2.getBase)
  }


  def changePlayer() : Unit = {
    if playerTurn == 1 then {
      currentPlayer = player2
      playerTurn = 2
    }
    else {
      currentPlayer = player1
      playerTurn = 1
    }
  }

  def clearScreen() : Unit = {
    for(i <- 0 until 20) {
      println("\n")
    }
  }

  def printBoard() : Unit = {
    printf("--------------------------------\n")
    print("     ")
    player2.printHousesReversed()
    print("\n")
    printf(s"[${player2.getBase}]                     [${player1.getBase}] ")
    print("\n     ")
    player1.printHouses()
    print("\n")
    printf("--------------------------------\n")
  }
  
  def makeCopy() : Board = {
    Board(player1.makeCopy(), player2.makeCopy())
  }
  
  def getCurrentPlayer() : Player = {
    currentPlayer
  } 
  
  def getPlayer1() : Player = {
    player1
  }
  def getPlayer2() : Player = {
    player2
  }
  
}
