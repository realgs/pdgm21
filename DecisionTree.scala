import scala.collection.mutable.ArrayBuffer

class DecisionTree(var game: KalahaGame, player: Player) {

  val root = new Node(game, player.getID(), Array(), -1)

    class Node(val currentGame: KalahaGame, val currentPlayer: Int, var children: Array[Node], val parent: Int) {

      val advantage = currentGame.advantage(player.getID())

      def createChildrenForRoot(currentGame: KalahaGame, currentPlayer: Int): Array[Node] =
        var children: Array[Node] = Array.fill(6)(null)
        val offset = if currentPlayer == 1 then 0 else 7

        for (i <- 0 to 5) {
          val game = new KalahaGame()
          var firstMove = i
          var nextPlayer = if currentPlayer == 1 then 2 else 1
          game.gameBoard.board = currentGame.gameBoard.board.clone()
          val move = game.makeMove(currentPlayer, i + offset)
          if move == -3 then firstMove = -3
          if move == 1 then nextPlayer = currentPlayer
          var child = new Node(game, nextPlayer, Array(), firstMove)
          children(i) = child
        }
        children

      def createChildren(currentGame: KalahaGame, currentPlayer: Int, firstMove: Int): Array[Node] =
        var children: Array[Node] = Array.fill(6)(null)
        val offset = if currentPlayer == 1 then 0 else 7

        for (i <- 0 to 5) {
          val game = new KalahaGame()
          var first = firstMove
          var nextPlayer = if currentPlayer == 1 then 2 else 1
          game.gameBoard.board = currentGame.gameBoard.board.clone()
          val move = game.makeMove(currentPlayer, i + offset)
          if move == -3 then first = -3
          if move == 1 then nextPlayer = currentPlayer
          var child = new Node(game, nextPlayer, Array(), first)
          children(i) = child
        }
        children
    }

  def calculate(depth: Int) =

    root.children = root.createChildrenForRoot(game, player.getID())

    def calculateHelper(childrenArray: Array[Node], currentDepth: Int, depth: Int): Unit =
      if currentDepth < depth then
        for (i <- 0 to 5)
            childrenArray(i).children = root.createChildren(childrenArray(i).currentGame, childrenArray(i).currentPlayer, childrenArray(i).parent)
            calculateHelper(childrenArray(i).children, currentDepth+1, depth)

    calculateHelper(root.children, 0, depth)

  def nodeArray(node: Node, result: ArrayBuffer[Node]): ArrayBuffer[Node] =
    if node.children.size != 0 then
      for (i <- 0 to 5)
        if node.children(i) != null then result.addOne(node.children(i))
        nodeArray(node.children(i), result)
    result

  def nodeList() = nodeArray(root, ArrayBuffer())

  def findBestMove() =
    val nodesArray = nodeList()
    var bestNode = nodesArray(0)
    var bestAdvantage = nodesArray(0).advantage

    for (i <- 0 to nodesArray.size - 1)
      if (nodesArray(i).advantage > bestAdvantage && nodesArray(i).parent != -3) || bestNode.parent == -3 then
      {
        bestNode = nodesArray(i)
        bestAdvantage = nodesArray(i).advantage
      }

    bestNode.parent

  def printNodes() =
    val list = nodeArray(root, ArrayBuffer())
    for (e <- list)
      println(e.currentGame.gameBoard.printBoard())
      println(e.advantage)
}
