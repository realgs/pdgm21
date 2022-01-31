import java.io.*
import java.net.*
import java.util.concurrent.TimeUnit
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, TimeoutException}
import scala.io.*
import scala.io.StdIn.readLine
import scala.util.control.Breaks.break

object Computer1 {
  def main(args: Array[String]) = {
    val s = new Socket(InetAddress.getByName("localhost"), 9991)
    val sString = new Socket(InetAddress.getByName("localhost"), 9993)
    lazy val in = new BufferedSource(s.getInputStream()).getLines()
    lazy val inString = new BufferedSource(sString.getInputStream()).getLines()
    val out = new PrintStream(s.getOutputStream())
    var board = new Board()

    while (true)
      try
        val line = in.next()
        println(line)
        if line.contains(":") then
          val test = inString.next()
          var board = Board().stringToObject(test)
          val best = board.bestSolution(0)
          out.println(best)
          println("TO:"+best)
          out.flush()
      catch
        case e: TimeoutException =>
          println("Przekroczyłeś czas odpowiedzi")
        case e: SocketException =>
          println("Koniec gry, rozłączono")
          s.close()
          sString.close()
          System.exit(0)
  }
}