class StrongEngine(var server: Server, id: Int) extends Player (id) {

  override def makeMove(currentGame: KalahaGame): Int =
    if id == 1 then println("Player 1: choose a cup between 0 and 5: ")
    else println("Player 2: choose a cup between 7 and 12: ")

    val tree = new DecisionTree(currentGame, this)
    tree.calculate(1)
    var chosenCup = tree.findBestMove()
    if id == 2 then chosenCup +=7

    Thread.sleep(3000)

    println(s"Player $id's choice: $chosenCup")
    chosenCup

  override def getID(): Int = id
}
