import java.net.*
import java.io.*
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration
import concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future, TimeoutException}
import scala.io.*
import scala.io.StdIn.readLine
import scala.util.control.Breaks.break
object User1 {
  def main(args: Array[String]) = {
    val s = new Socket(InetAddress.getByName("localhost"), 9991)
    val s1 = new Socket(InetAddress.getByName("localhost"), 9993)
    val in = new BufferedSource(s.getInputStream()).getLines()
    val out = new PrintStream(s.getOutputStream())


    while (true)
      try
        val line = in.next()
        println(line)
        if line.contains(":") then
          out.println(readLine())
          out.flush()
      catch
        case e: TimeoutException =>
          println("Przekroczyłeś czas odpowiedzi")
        case e: SocketException =>
          println("Koniec gry, rozłączono")
          s.close()
          s1.close()
          System.exit(0)
  }
}