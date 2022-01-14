package pdgm21

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class Node (var children: Array[Node], var gameState: GameState,currentPlayer:Int,val parentIndex:Int):
  val diffrence =
    if currentPlayer==1 then gameState.gameBoard(Player1BaseIndex) - gameState.gameBoard(Player2BaseIndex)
    else
      gameState.gameBoard(Player2BaseIndex) - gameState.gameBoard(Player1BaseIndex)


class PlayerAI(var server: Server,playerId:Int) extends Player:




  def getBest(list: List[Node]):Node = {
    return list.apply(2)
  }
/*
*
*
*
*
*
* */
// they have to have diffrent parentIndex Vaules because they are the actual parents
//
  def createRootChildren(gameState: GameState):Array[Node] = {
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





//    val game  = new Game
//    val gameboard = gameState.gameBoard
//
//    game.setBoard(gameState.gameBoard)
//    game.printGameStatus()
//    val currentPlayer = gameState.nextPlayer
//    val indexOffset  = if(currentPlayer==2) then 7 else 0
//    var node0 = new Node(Array(),game.move(0+indexOffset,currentPlayer),0)
//    game.printGameStatus()
//    game.setBoard(gameboard)
//    var node1 = new Node(Array(),game.move(1+indexOffset,currentPlayer),1)
//    game.printGameStatus()
//    game.setBoard(gameboard)
//    var node2 = new Node(Array(),game.move(2+indexOffset,currentPlayer),2)
//    game.printGameStatus()
//    game.setBoard(gameState.gameBoard)
//    var node3 = new Node(Array(),game.move(3+indexOffset,currentPlayer),3)
//    game.printGameStatus()
//    game.setBoard(gameState.gameBoard)
//    var node4 = new Node(Array(),game.move(4+indexOffset,currentPlayer),4)
//    game.printGameStatus()
//    game.setBoard(gameState.gameBoard)
//    var node5 = new Node(Array(),game.move(5+indexOffset,currentPlayer),5)
//    game.printGameStatus()
//    game.setBoard(gameState.gameBoard)
//    return Array(node0,node1,node2,node3,node4,node5)
  }



  def createList(gameState: GameState,parentIndex:Int):Array[Node] = {
//    var game = new Game
//    val currentPlayer = gameState.nextPlayer
//    val indexOffset  = if(currentPlayer==2) then 7 else 0
//    var children = Array.empty[Node]
//    for(i<-0 to 5)
//      var node = new Node(Array(),game.move(i+indexOffset,currentPlayer),currentPlayer,parentIndex)
//      game.printGameStatus()
//      println(node.diffrence)
//      game.setBoard(gameState.gameBoard.clone())
//      children :+ node
//
//    println(children.toList)
//    children
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
//    val game  = new Game
//    game.setBoard(gameState.gameBoard)
//    val currentPlayer = gameState.nextPlayer
//    //println(currentPlayer)
//
//
//    val indexOffset  = if(currentPlayer==2) then 7 else 0
//    var node0 = new Node(Array(),game.move(0+indexOffset,currentPlayer),parentIndex)
//
//    game.setBoard(gameState.gameBoard)
//    var node1 = new Node(Array(),game.move(1+indexOffset,currentPlayer),parentIndex)
//    game.setBoard(gameState.gameBoard)
//    var node2 = new Node(Array(),game.move(2+indexOffset,currentPlayer),parentIndex)
//    game.setBoard(gameState.gameBoard)
//    var node3 = new Node(Array(),game.move(3+indexOffset,currentPlayer),parentIndex)
//    var node4 = new Node(Array(),game.move(4+indexOffset,currentPlayer),parentIndex)
//    game.setBoard(gameState.gameBoard)
//    var node5 = new Node(Array(),game.move(5+indexOffset,currentPlayer),parentIndex)
//    game.setBoard(gameState.gameBoard)
//
//    return Array(node0,node1,node2,node3,node4,node5)
//  }

  def makeTree(depth:Int,gameState: GameState):Node={

    var root = new Node(createRootChildren(gameState),gameState,gameState.nextPlayer,-2)
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
// i know that it can probably be done without creating new list but i could'nt figure it how


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


  def makeMove(gameState: GameState): Int ={
    val root = makeTree(4,gameState)
    val array = treeToArray(root,ArrayBuffer())
    //println(array.length)
    val bestMove = getBestMove(array,gameState)
    //println(gameState.gameBoard(bestMove))
    if(getId()==1)
      bestMove
    else
      bestMove+7
  }

  def getId():Int={
      return playerId
  }


