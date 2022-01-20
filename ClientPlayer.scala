import scala.io.StdIn.readLine
import scala.concurrent.duration.Duration
import scala.concurrent.{ExecutionContext, Await, Future, TimeoutException}
class ClientPlayer(ID: String, hasCurrentTurn:Boolean) extends Client(ID, hasCurrentTurn) {

  val P1 = "P1";
  val P2 = "P2";

  def chooseHouse(game: Game): Int = {
    implicit val execution = ExecutionContext.global
    @volatile var inputStr = ""
    println(ID + ", it's your turn!")
    if (ID == P1) then println("Choose hole from 1 to 6 ") else println("Choose hole from 8 to 13")

    try {

      //Future{wait(2000); if (input == -1) then -1 else input }
      inputStr = readLine
      var input = inputStr.toInt
      if (ID == P1) {
        //if (input == )
        if (input >= 1 && input <= 6) {                                     //if fits the range of player1 holes
          if (!game.isHouseEmpty(input - 1)) {                              //if the hole is not empty
            (input - 1)
          } else {
            println("You can't choose an empty hole!")
            chooseHouse(game)
          }
        } else {
          println("Error! Enter a number from 1 to 6")
          chooseHouse(game)
        }
      } else if (ID == P2) {
        if (input >= 8 && input <= 13) {                                   //if fits the range of player2 holes
          if (!game.isHouseEmpty(input - 1)) {                              //if the hole is not empty
            (input - 1)
          } else {
            println("You can't choose an empty hole!")
            chooseHouse(game)
          }
        } else {
          println("Error! Enter a number from 8 to 13")
          chooseHouse(game)
        }
      } else {
        println("This should not happen.")
        chooseHouse(game)
      }
    } catch {
      case exception: NumberFormatException =>                              //if entered NaN
        if (inputStr == "") {
          return -1
        }
        else {
          println("Enter a number")
          game.printCurrentBoard()
          chooseHouse(game)
        }

    }
  }
}