import scala.util.Random

class RandomComputer(var server: Server, id: Int) extends Player (id) {

  val random = new Random()

  override def makeMove(currentGame: KalahaGame): Int =
    if id == 1 then println("Player 1: choose a cup between 0 and 5: ")
    else println("Player 2: choose a cup between 7 and 12: ")
    var chosenCup = random.between(0, 6)
    if id == 2 then chosenCup += 7

    while (currentGame.gameBoard.getStones(chosenCup) == 0)
      {
        chosenCup = random.between(0, 6)
        if id == 2 then chosenCup += 7
      }

    Thread.sleep(3000)

    println(s"Player $id's choice: $chosenCup")
    chosenCup

    chosenCup

  override def getID(): Int = id
}
