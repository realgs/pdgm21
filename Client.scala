import java.io.PrintStream

abstract class Client(val name : String, val numberOfPlayer : Int, val inStream:Iterator[String], val outStream:PrintStream):
  def getName = name
  def getNumberOfPlayer = numberOfPlayer
  def selectHole(board: Board) : Int


