import java.io.PrintStream
import scala.io.StdIn.*


class Player(name: String, number: Int,inStream:Iterator[String],outStream:PrintStream) extends Client(name, number, inStream, outStream):
  override def selectHole(board: Board): Int = 
    var hole = readInt()
    if number==0 then
      hole +=1
    else
      hole = 13-hole 
      
    while(!board.validMove(hole,number))
      println("Ruch nie moze zostac wykonany, wybierz inny ruch")
      hole = readInt()
      if number==0 then
        hole +=1
      else
        hole = 13-hole
    
    hole



  



