import java.net.*
import java.io.*
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration
import concurrent.ExecutionContext.Implicits.global
import scala.io.*
import scala.io.StdIn.readLine
import scala.concurrent.{Await, Future, TimeoutException}
import scala.util.control.Breaks.break

object User2 {
  def main(args: Array[String]) = {
    val s = new Socket(InetAddress.getByName("localhost"), 9992)
    val s1 = new Socket(InetAddress.getByName("localhost"), 9994)
    lazy val in = new BufferedSource(s.getInputStream()).getLines()
    val out = new PrintStream(s.getOutputStream())

    while (true)
      try
        val future = Future{
          val line = in.next()
          println(line)
          if line.contains(":") then
            val test = readLine()
            out.println(test)
            out.flush()
       }
        Await.result(future, Duration(15, TimeUnit.SECONDS))
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
