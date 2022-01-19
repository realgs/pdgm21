import scala.io.StdIn.readInt

class HumanPlayer(var server: Server, id: Int) extends Player(id) {

  override def makeMove(currentGame: KalahaGame): Int =
    if id == 1 then println("Player 1: choose a cup between 0 and 5: ")
    else println("Player 2: choose a cup between 7 and 12: ")
    val chosenCup = readInt()
    if id == 1 then
      if chosenCup<0 || chosenCup>5 then {
        println("Invalid cup, try again!")
        return -1
      }
      else if currentGame.gameBoard.getStones(chosenCup) == 0 then {
        println("Cup is empty, try again!")
        return -2
      }
      else chosenCup
    else
      if chosenCup<7 || chosenCup>12 then {
        println("Invalid cup, try again!")
        return -1
      }
      else if currentGame.gameBoard.getStones(chosenCup) == 0 then {
        println("Cup is empty, try again!")
        return -2
      }
      else chosenCup

  override def getID(): Int = id
}
