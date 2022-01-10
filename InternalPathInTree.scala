import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import Utils.{time, parallel}

object InternalPathInTree {

  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  def generateNDepthTree(N: Int): BT[Int] =
    if N > 0 then Node(scala.util.Random.nextInt(), generateNDepthTree(N - 1), generateNDepthTree((N - 1)))
    else Empty


  def internalPath[A](bt: BT[A], startDepth: Int): Int =
    def internalPathRec[A](bt: BT[A], depth: Int): Int =
      bt match
        case Empty => 0
        case Node(v, l, r) => depth + internalPathRec(l, depth + 1) + internalPathRec(r, depth + 1)
    internalPathRec(bt, startDepth)


  def internalPathFuture[A](bt: BT[A], threshold: Int): Int =
    def internalPathRec[A](bt: BT[A], depth: Int): Int =
      if depth >= threshold then internalPath(bt, depth)
      else
        bt match
          case Empty => 0
          case Node(v, l, r) =>
            val left = Future {internalPathRec(l, depth + 1)}
            val right = Future {internalPathRec(r, depth + 1)}
            depth + Await.result(left, Duration.Inf) + Await.result(right, Duration.Inf)
    internalPathRec(bt, 0)


  def internalPathParallel[A](bt: BT[A], threshold: Int): Int =
    def internalPathRec[A](bt: BT[A], depth: Int): Int =
      if depth >= threshold then internalPath(bt, depth)
      else
        bt match
          case Empty => 0
          case Node(v, l, r) =>
            val (left, right) = parallel(internalPathRec(l, depth + 1), internalPathRec(r, depth + 1))
            depth + left + right
    internalPathRec(bt, 0)



  def main(args: Array[String]): Unit = {

    // sciezka wewnetrzna to suma glebokosci wezlow wewnetrznych, gdzie glebokosc wezla to liczba krawedzi od korzenia do wezla)
    println("Testy sciezki wewnetrznej w drzewie:")
    println()

    println("Testy poprawnosci:")
    var tree = generateNDepthTree(3)
    println("normal:   " + (internalPath(tree, 0) == 10))
    println("future:   " + (internalPathFuture(tree, 2) == 10))
    println("parallel: " + (internalPathParallel(tree, 2) == 10))
    println()


    println("Testy przy zmiennym threshold i stalej glebokosci drzewa:")
    tree = generateNDepthTree(20)

    println("depth: 20, threshold: 1")
    println("normal:   " + time(internalPath(tree, 0)))
    println("future:   " + time(internalPathFuture(tree, 1)))
    println("parallel: " + time(internalPathParallel(tree, 1)))
    println()

    println("depth: 20, threshold: 2")
    println("normal:   " + time(internalPath(tree, 0)))
    println("future:   " + time(internalPathFuture(tree, 2)))
    println("parallel: " + time(internalPathParallel(tree, 2)))
    println()

    println("depth: 20, threshold: 3")
    println("normal:   " + time(internalPath(tree, 0)))
    println("future:   " + time(internalPathFuture(tree, 3)))
    println("parallel: " + time(internalPathParallel(tree, 3)))
    println()

    println("depth: 20, threshold: 4")
    println("normal:   " + time(internalPath(tree, 0)))
    println("future:   " + time(internalPathFuture(tree, 4)))
    println("parallel: " + time(internalPathParallel(tree, 4)))
    println()

    println("depth: 20, threshold: 5")
    println("normal:   " + time(internalPath(tree, 0)))
    println("future:   " + time(internalPathFuture(tree, 5)))
    println("parallel: " + time(internalPathParallel(tree, 5)))
    println()

    println("depth: 20, threshold: 6")
    println("normal:   " + time(internalPath(tree, 0)))
    println("future:   " + time(internalPathFuture(tree, 6)))
    println("parallel: " + time(internalPathParallel(tree, 6)))
    println()

    println("depth: 20, threshold: 7")
    println("normal:   " + time(internalPath(tree, 0)))
    println("future:   " + time(internalPathFuture(tree, 7)))
    println("parallel: " + time(internalPathParallel(tree, 7)))
    println()

    println("depth: 20, threshold: 8")
    println("normal:   " + time(internalPath(tree, 0)))
    println("future:   " + time(internalPathFuture(tree, 8)))
    println("parallel: " + time(internalPathParallel(tree, 8)))
    println()

    println("depth: 20, threshold: 9")
    println("normal:   " + time(internalPath(tree, 0)))
    println("future:   " + time(internalPathFuture(tree, 9)))
    println("parallel: " + time(internalPathParallel(tree, 9)))
    println()


    println("Testy przy stalym threshold i zmiennej glebokosci drzewa:")
    tree = generateNDepthTree(5)
    println("depth: 5, threshold: 1")
    println("normal:   " + time(internalPath(tree, 0)))
    println("future:   " + time(internalPathFuture(tree, 1)))
    println("parallel: " + time(internalPathParallel(tree, 1)))
    println()

    tree = generateNDepthTree(10)
    println("depth: 10, threshold: 1")
    println("normal:   " + time(internalPath(tree, 0)))
    println("future:   " + time(internalPathFuture(tree, 1)))
    println("parallel: " + time(internalPathParallel(tree, 1)))
    println()

    tree = generateNDepthTree(15)
    println("depth: 15, threshold: 1")
    println("normal:   " + time(internalPath(tree, 0)))
    println("future:   " + time(internalPathFuture(tree, 1)))
    println("parallel: " + time(internalPathParallel(tree, 1)))
    println()

    tree = generateNDepthTree(17)
    println("depth: 17, threshold: 1")
    println("normal:   " + time(internalPath(tree, 0)))
    println("future:   " + time(internalPathFuture(tree, 1)))
    println("parallel: " + time(internalPathParallel(tree, 1)))
    println()

    tree = generateNDepthTree(20)
    println("depth: 20, threshold: 1")
    println("normal:   " + time(internalPath(tree, 0)))
    println("future:   " + time(internalPathFuture(tree, 1)))
    println("parallel: " + time(internalPathParallel(tree, 1)))
    println()

    tree = generateNDepthTree(22)
    println("depth: 22, threshold: 1")
    println("normal:   " + time(internalPath(tree, 0)))
    println("future:   " + time(internalPathFuture(tree, 1)))
    println("parallel: " + time(internalPathParallel(tree, 1)))
    println()

    tree = generateNDepthTree(25)
    println("depth: 25, threshold: 1")
    println("normal:   " + time(internalPath(tree, 0)))
    println("future:   " + time(internalPathFuture(tree, 1)))
    println("parallel: " + time(internalPathParallel(tree, 1)))
    println()


  }
}

