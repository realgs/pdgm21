import model.GameSpecification
import model.serverModel.Server
import views.MainView

import java.util.concurrent.TimeoutException
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

object ServerKalah:
  def main(args: Array[String]): Unit =
    print("Podaj ilosc kamieni na start: ")
    val input = Future{scala.io.StdIn.readInt()}
    try
      val amountOfRocks = Await.result(input, Duration(30, "seconds"))
      GameSpecification.resetAmountOfRocks(amountOfRocks)
    catch
      case e: TimeoutException => System.exit(0)


    val server = new Server(GameSpecification.PORT)
    server.startServer()

