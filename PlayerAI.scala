package pdgm21

import scala.util.Random

class PlayerAI(var server: Server,playerId:Int) extends Player:

//TODO create something better than random

  case class Node (var children: Array[Node], val value:Int)

  def getBest(list: List[Node]):Node = {
    return list.apply(2)
  }

  def createList():Array[Node] = {
    var node0 = new Node(Array(),0)
    var node1 = new Node(Array(),1)
    var node2 = new Node(Array(),2)
    var node3 = new Node(Array(),3)
    var node4 = new Node(Array(),4)
    var node5 = new Node(Array(),5)
    return Array(node0,node1,node2,node3,node4,node5)
  }

  def makeTree(depth:Int):Node={
    var root = new Node(createList(),1)
    def helper(list: Array[Node],depth:Int,currentDepth:Int):Unit = {
      if(depth!=currentDepth) then
        for(i<-0 to 5)
          list(i).children= createList()
          helper(list(i).children,depth,currentDepth+1)
      else
        return
    }
    helper(root.children,3,0)
    return root
  }




  def makeMove(gameState: GameState): Int ={
    if(getId()==1)
      2
    else
      7
  }

  def getId():Int={
      return playerId
  }


