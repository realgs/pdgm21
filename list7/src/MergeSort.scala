import scala.annotation.tailrec
import ParallelResearchUtils.*

object MergeSort {
  // Widoczna przewaga zrównoleglonego rozwiązania od 100 000 danych przy progu 50 000 - czas dwa razy krótszy (78ms w porównaniu do 141ms)
  // dla mniejszych danych wyniki algorytmów są porównywalne dla 50 000 danych przy progu 10 000 (63ms dla równoległego, 78ms dla zwykłego)
  // przy mniejszych lepsze rozwiązanie jednowątkowe

  val lengthThreshold = 10000
  val rand = scala.util.Random()

  def generateList(length: Int, acc: List[Int]): List[Int] = {
    if length < 0 then throw new IllegalArgumentException()
    else if length == 0 then acc
    else generateList(length - 1, rand.nextInt() :: acc)
  }
     val isLess :  (Int, Int) => Boolean = (arg1, arg2) => (arg1 < arg2)

    def splitList[A](list : List[A], partition : Int) : (List[A], List[A]) ={
      @tailrec
      def splitIter(sublists :(List[A], List[A]), counter : Int) : (List[A], List[A]) ={
        if counter == 0 then (sublists._1.reverse, sublists._2)
        else splitIter((sublists._2.head :: sublists._1, sublists._2.tail), counter - 1)
      }
      splitIter((Nil, list), partition)
    }

    def merge[A](order: (A, A) => Boolean, listLeft: List[A], listRight: List[A]): List[A] = {
      def mergeHelper(lleft: List[A], lright: List[A], acc: List[A]): List[A] = {
        (lleft, lright) match
          case (Nil, _) => lright.reverse ::: acc
          case (_, Nil) => lleft.reverse ::: acc
          case (h1 :: t1 , h2 :: t2) =>
            if order(h2, h1) then mergeHelper(lleft, t2, h2 :: acc)
            else mergeHelper(t1, lright, h1 :: acc)
      }
      mergeHelper(listLeft, listRight, Nil).reverse
    }

    def mergesort[A](order: (A, A) => Boolean,list : List[A]) : List[A] = {
      val partition = list.length / 2
      if partition == 0 then list
      else {
        val (left, right): (List[A], List[A]) = splitList(list, partition)
        merge(order, mergesort(order, left), mergesort(order, right))
      }
    }

    def mergesortParallel[A](order: (A, A) => Boolean,list : List[A]) : List[A] ={
      val length = list.length
      val partition = length / 2
      if length < 2 then list
      else {
        val (left, right): (List[A], List[A]) = splitList(list, partition)
        if length < lengthThreshold then merge(order, left, right)
        else {
          val (r1, r2): (List[A], List[A]) = parallel(mergesortParallel(order, left), mergesortParallel(order, right))
          merge(order, r1, r2)
        }
      }
    }
  }
