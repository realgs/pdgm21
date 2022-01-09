package Lab7

import scala.concurrent.{Await, Future}
import concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.io.Source


object LineInText {

  def readFile(fileName: String): Int = {
    var i = 0
    for (line <- Source.fromFile(fileName).getLines) {
      i = i + 1;
    }
    i
  }

  def text(fileName: String, fileName2: String): Int = {

    val number = readFile(fileName)
    val number2 = readFile(fileName2)
    if number > number2 then number
    else number2

  }

  def textFuture(fileName: String, fileName2: String): Int = {
    val fut1 = Future(readFile(fileName))
    val fut2 = Future(readFile(fileName2))

    val el1 = Await.result(fut1, Duration.Inf)
    val el2 = Await.result(fut2, Duration.Inf)
    if el1 > el2 then el1
    else el2

  }


}
