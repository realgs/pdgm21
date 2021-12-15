package main

import scala.annotation.tailrec

trait Debug {
  // zad 4
  def debugName(): String =
    this.getClass.getName

  // zad 5
  def debugVars(): String =
    val arr = this.getClass.getDeclaredFields()
    def vari(fi:java.lang.reflect.Field): String =
      fi.setAccessible(true)
      var pom = fi.get(this)
      val result = "["+fi.getName+", "+fi.getType.getName+", "+pom.toString+"]"
      fi.setAccessible(false)
      result
    def step(fi:Array[java.lang.reflect.Field]): String =
      if fi.tail.isEmpty then vari(fi.head)
      else vari(fi.head)+", "+step(fi.tail)
    if arr.isEmpty then "[]" else "["+step(arr)+"]"
}
class Point(xv: Int, yv: Int) extends Debug {
  var x: Int = xv
  var y: Int = yv
  var a: String = "test"
}

object Main {
  def main(args: Array[String]): Unit = {
    // zad 1
    def eachNElement(numbers:LazyList[Int], n:Int, m:Int): LazyList[Int] =
      def step(numbers:LazyList[Int], count:Int, end:Int): LazyList[Int] =
        (count == n, end <= 1) match
          case (true, true) => numbers.head+:step(LazyList(), 0, 0)
          case (false, true) => LazyList()
          case (true, false) => numbers.head+:step(numbers.tail, 1, end-1)
          case (false, false) => step(numbers.tail, count+1, end-1)
      step(numbers, n, m)

    // zad 2
    def lazyExecute(list1:LazyList[Int], list2:LazyList[Int], oper:(Int, Int)=>Int): LazyList[Int] =
      def step(list1:LazyList[Int], list2:LazyList[Int]): LazyList[Int] =
        (list1, list2) match
          case (LazyList(), LazyList()) => LazyList()
          case (h1+:t1, LazyList()) => h1+:step(t1, LazyList())
          case (LazyList(), h2+:t2) => h2+:step(LazyList(), t2)
          case (h1+:t1, h2+:t2) => oper(h1, h2)+:step(t1, t2)
      step(list1, list2)


    // zad 3
    def kollection[A](koll1:List[A], koll2:List[Int]): List[A] =
      def add(koll1:List[A], koll2:List[Int], count:Int): List[A] =
        if koll1 == Nil || koll2 == Nil then Nil else
          if count == koll2.head then add(koll1.tail, koll2.tail, 0)
          else koll1.head::add(koll1, koll2, count+1)
      add(koll1, koll2, 0)



    def listPrinter(l:LazyList[Int], n:Int): Unit =
      if l == LazyList() || n==0 then println()
      else
        print(l.head + ", ")
        listPrinter(l.tail, n-1)

    // testy do 1
    print("[5;6;3;2;1], 2, 3 -> ")
    val test1:LazyList[Int] = eachNElement(LazyList(5, 6, 3, 2, 1), 2, 3)
    listPrinter(test1, 9000)
    print("[5;6;3;2;1], 2, 4 -> ")
    val test21:LazyList[Int] = eachNElement(LazyList(5, 6, 3, 2, 1), 2, 4)
    listPrinter(test21, 9000)
    print("[5;6;3;2;1], 2, 5 -> ")
    val test2:LazyList[Int] = eachNElement(LazyList(5, 6, 3, 2, 1), 2, 5)
    listPrinter(test2, 9000)
    //testy do 2
    print("zad 2\n[1;2;3], [2;3;4;5] oraz + -> ")
    val test3:LazyList[Int] = lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), _ + _)
    listPrinter(test3, 9000)
    print("[1;2;3], [2;3;4;5] oraz - -> ")
    val test4:LazyList[Int] = lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), _ - _)
    listPrinter(test4, 9000)
    print("[1;2;3], [2;3;4;5] oraz * -> ")
    val test5:LazyList[Int] = lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), _ * _)
    listPrinter(test5, 9000)
    print("[1;2;3], [2;3;4;5] oraz / -> ")
    val test6:LazyList[Int] = lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), _ / _)
    listPrinter(test6, 9000)
    //testy do 3
    print("zad 3\ndla [1;2;3] oraz [0;3;1;4] -> ")
    println(kollection(List(1, 2, 3), List(0, 3, 1, 3)).mkString(", "))
    //testy do 4
    var p : Point = new Point(3, 4);
    println("zad 4\n" + p.debugName());
    //testy do 5
    println("zad 5\n" + p.debugVars())
  }
}
