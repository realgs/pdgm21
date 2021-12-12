import java.util

object Main {
  def main(args: Array[String]): Unit = {
    val list = LazyList(5,4,3,2,1)
    val result = eachNElement(list,5,2).toList
    println(result)

    val first = LazyList(5,2,0)
    val second = LazyList(2,3,4,5)
    val addResult = lazyExecute(first,second,'/')
    println(addResult.toList)


    val listToDuplicate = List(1,2,3)
    val duplicators = List(2,2,2)

    val duplicatedResult = duplicate(listToDuplicate,duplicators)
    println(duplicatedResult)
  }
}


def eachNElement[A](list :LazyList[A], m :Int, n :Int) =
  def eachNElementHelper[A](list :LazyList[A], currentM :Int, currentN :Int): LazyList[A] =
    (list,currentM) match
      case (LazyList(),_) => LazyList()
      case (h #:: t, 0) => LazyList()
      case (h #:: t,_) =>
        if currentN == 1 then h #:: eachNElementHelper(t, currentM - 1, n)
        else  eachNElementHelper(t, currentM - 1, currentN - 1)


  eachNElementHelper(list, m, 1)




def lazyExecute(first: LazyList[Int], second: LazyList[Int], symbol :Char) =
  symbol match
    case '+' => (first.zipAll(second,0,0)).map((f,s) => f + s)
    case '-' => (first.zipAll(second,0,0)).map((f,s) => f - s)
    case '*' => (first.zipAll(second,0,0)).map((f,s) => f * s)
    case '/' => (first.zipAll(second,0,0)).map((f,s) => f / s)
    case _ => LazyList()


def copy[A](element: A, number: Int): List[A] =
  if number > 0 then element :: copy(element, number-1)
  else Nil


def duplicate[A](list: List[A], copyNumbers :List[Int]): List[A] =
  (list zip copyNumbers).flatMap((a,n) => copy(a,n))










