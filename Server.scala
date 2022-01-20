import java.util.concurrent.TimeoutException
import scala.concurrent.duration.{Duration, SECONDS}
import scala.concurrent.{Await, Future, Promise, TimeoutException}
import concurrent.ExecutionContext.Implicits.global
import scala.annotation.tailrec

class Server(p1:Player, p2:Player) {
  private val game = KalahaSim()
  private val player1 = p1
  private val player2 = p2

  def start(): Unit = round(true)

  private def round(p1:Boolean): Unit =      // function responsible for a single round
    Interface.showBoard(game)
    if (p1 && player1.noMove()) || (!p1 && player2.noMove()) then
      val score = game.pointDiff()
      Interface.victory(if score<0 then 2 else if score>0 then 1 else 0)
    else
      val fx = Future { (if p1 then player1 else player2).moveOwn() }
      try
        val x = Await.result(fx, Duration(30, SECONDS))
        println(x)
        val moveResult = game.move(x, p1)
        player1.move(x, p1)
        player2.move(x, p1)
        if moveResult == 1 then round(p1) else round(!p1)
      catch
        case e:TimeoutException =>
          (if p1 then player1 else player2).skipFix()
          round(!p1)

}
