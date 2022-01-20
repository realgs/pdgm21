import scala.concurrent.{Await, Future}
import concurrent.ExecutionContext.Implicits.global
import scala.annotation.tailrec
import scala.concurrent.duration.Duration

class AIplayer(override val Nr1: Boolean) extends Player(Nr1)  {

  protected class Move(val board:KalahaSim, val move:Int, val nextP1:Boolean)

  private var deep = 6

  protected sealed trait Tree
  case object End extends Tree
  case class Play(board:KalahaSim, options:()=>Array[Tree]) extends Tree

  private def genTree(board:KalahaSim, poz:Int): Tree =
    if board == null || board.at(poz) == 0 then End else
      val boardNew = board.cloner()
      val x = boardNew.move(poz, poz < 6)
      val i = if (poz < 6) == (x == 1) then 0 else 7
      Play(boardNew, ()=>Array(genTree(boardNew, i), genTree(boardNew, i+1),genTree(boardNew, i+2),genTree(boardNew, i+3),genTree(boardNew, i+4),genTree(boardNew, i+5)))

  private def findWay(): Int =
    def findBestChild(tree: Tree, depth:Int): Int =
      tree match
        case End | null => -999
        case Play(board:KalahaSim, options:(()=>Array[Tree])) =>
          if depth == 0 then (if Nr1 then board.pointDiff() else 0-board.pointDiff()) else
            @tailrec
            def max(list: List[Int], best:(Int, Int), now:Int): (Int, Int) =
              if list.isEmpty then best else max(list.tail, if best._1<list.head then (list.head, now) else best, now-1)
            var list = if depth <=deep-2 then List(findBestChild(options()(0), depth-1),findBestChild(options()(1), depth-1),findBestChild(options()(2), depth-1),findBestChild(options()(3), depth-1),findBestChild(options()(4), depth-1),findBestChild(options()(5), depth-1))
            else
              val f0 = Future { findBestChild(options()(0), depth-1) }
              val f1 = Future { findBestChild(options()(1), depth-1) }
              val f2 = Future { findBestChild(options()(2), depth-1) }
              val f3 = Future { findBestChild(options()(3), depth-1) }
              val f4 = Future { findBestChild(options()(4), depth-1) }
              val f5 = Future { findBestChild(options()(5), depth-1) }
              List(Await.result(f0, Duration.Inf),Await.result(f1, Duration.Inf),Await.result(f2, Duration.Inf),Await.result(f3, Duration.Inf),Await.result(f4, Duration.Inf),Await.result(f5, Duration.Inf))
            if Nr1 then list = list.reverse
            val (x, y) = max(list, (-999, -999), 5)
            if x == -999 then (if Nr1 then board.pointDiff() else 0-board.pointDiff()) else if depth == deep then y else x
    val i = if Nr1 then 0 else 7
    val f0 = Future { genTree(game, i) }
    val f1 = Future { genTree(game, i+1) }
    val f2 = Future { genTree(game, i+2) }
    val f3 = Future { genTree(game, i+3) }
    val f4 = Future { genTree(game, i+4) }
    val f5 = Future { genTree(game, i+5) }
    findBestChild(Play(game, ()=>Array(Await.result(f0, Duration.Inf),Await.result(f1, Duration.Inf),Await.result(f2, Duration.Inf),Await.result(f3, Duration.Inf),Await.result(f4, Duration.Inf),Await.result(f5, Duration.Inf))), deep)

  override def moveOwn(): Int =
    val move = if Nr1 then findWay() else 12-findWay()
    if move < 0 then throw Exception("AI tried illegal move") else
      Interface.computerMove(Nr1, move)
      move

  override def skipFix(): Unit =
    if deep > 3 then deep = deep - 1
    Interface.missTurn()
    print("1")
    println(if Nr1 then 1 else 2)
}
