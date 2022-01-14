package pdgm21

import scala.collection.mutable.ArrayBuffer
import scala.util.Random


// parent index is an actual move (hole) so every node has exactly 6 children
class Node (var children: Array[Node], var gameState: GameState,currentPlayer:Int,val parentIndex:Int):
  
  // diffrence that we want to maximize in decision tree
  val diffrence =
    if currentPlayer==1 then gameState.gameBoard(Player1Mancala) - gameState.gameBoard(Player2Mancala)
    else
      gameState.gameBoard(Player2Mancala) - gameState.gameBoard(Player1Mancala)


class PlayerAI(var server: Server,playerId:Int) extends Player:
  

  def createRootChildren(gameState: GameState):Array[Node] = {
    // here we are creating list of root's children so they have to have parentIndex of themself
    var game = new Game
    val currentPlayer = gameState.nextPlayer
    val indexOffset  = if(currentPlayer==2) then 7 else 0
    var children :Array[Node] = Array.fill(6)(null)
    game.setBoard(gameState.gameBoard.clone())
    
    for(i<-0 to 5)
      var node = new Node(Array(),game.move(i+indexOffset,currentPlayer),currentPlayer,i)
      game.setBoard(gameState.gameBoard.clone())
      children(i)=node
    children
  }



  def createList(gameState: GameState,parentIndex:Int):Array[Node] = {
    
    var game = new Game
    val currentPlayer = gameState.nextPlayer
    val indexOffset  = if(currentPlayer==2) then 7 else 0
    var children :Array[Node] = Array.fill(6)(null)
    game.setBoard(gameState.gameBoard.clone())
    
    for(i<-0 to 5)
      var node = new Node(Array(),game.move(i+indexOffset,currentPlayer),currentPlayer,parentIndex)
      game.setBoard(gameState.gameBoard.clone())
      children(i)=node
    children




  }


  def makeTree(depth:Int,gameState: GameState):Node={

    var root = new Node(createRootChildren(gameState),gameState,gameState.nextPlayer,-2) // some inital random value that should be wrong
    def helper(list: Array[Node],depth:Int,currentDepth:Int):Unit = {
      if(depth!=currentDepth) then
        for(i<-0 to 5)
          list(i).children= createList(list(i).gameState,list(i).parentIndex)
          helper(list(i).children,depth,currentDepth+1)
      else
        return
    }
    helper(root.children,3,0)
    return root
  }

// to make the search for best move easier 
  def treeToArray(node: Node,arrayBuffer: ArrayBuffer[Node]):ArrayBuffer[Node] = {
    if(node.children.size!=0) then
      for(i<- 0 to 5)
        arrayBuffer.addOne(node.children(i))
        treeToArray(node.children(i),arrayBuffer)
    arrayBuffer
  }

  def getBestMove(arrayBuffer: ArrayBuffer[Node],gameState: GameState):Int = {
    
    var bestDiff = arrayBuffer.apply(0).diffrence
    var bestNode = arrayBuffer.apply(0)
    val indexOffset = if gameState.nextPlayer==2 then 7 else 0
    
    for(elem<- arrayBuffer)
      if elem.diffrence>bestDiff&& gameState.gameBoard(elem.parentIndex+indexOffset)!=0 then
        bestDiff = elem.diffrence
        bestNode = elem
    bestNode.parentIndex
  }


  override def makeMove(gameState: GameState): Int ={
    val root = makeTree(4,gameState)
    val array = treeToArray(root,ArrayBuffer())
    val bestMove = getBestMove(array,gameState)
    if(getId()==1)
      bestMove
    else
      bestMove+7
  }
  
  
  override def getId():Int={
      return playerId
  }


