package Client
import Game.Board

abstract class Client(private val name : String, private val numberOfPlayer : Int) {
  def getName = name
  def getNumberOfPlayer = numberOfPlayer
  def whichHole(board : Board) : Int

}
