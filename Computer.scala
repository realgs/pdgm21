package kalahgame

class Computer(val depth: Int) extends Player:

  private var identity = 0

  def ID: Int = identity

  def ID_=(newId: Int): Unit =
    identity = newId

  def pickMove(board: Board): Int =
    print("\t\t\t\t\t\t\t\t\tPlayer" + identity + " choose pit: ")
    val tree = new DecisionTree
    val minimax = new MinimaxDecision
    val choice = minimax.getBestMove(tree.generateDecisionTree(board, depth, identity), identity)._2
    println(choice)
    choice

end Computer

