
object Lista2 {
  def main(args: Array[String]): Unit = {

    println("zad.1");
    def sum(list: List[Int]): Int = {
      def sumIter(list: List[Int], acc: Int): Int =
        if list == Nil then acc
        else sumIter(list.tail, acc + list.head)

      sumIter(list, 0)
    }

    println(sum(List()));
    println(sum(List(1,2,3,4,5)));
    println(sum(List(-1,-2,2,1,0)));

    println("zad.2");
    def sentencer(words: List[String]): String =
      def sentencerIter(words: List[String],sentence: String): String =
        if words == Nil then sentence + "."
        else if words.head == "." || words.head == "?" || words.head == "!" then sentence + words.head
        else sentencerIter(words.tail,sentence + words.head + " ")
      sentencerIter(words,"")

    println(sentencer(List("Ala","ma","kota",".")))
    println(sentencer(List("Tomek","ma","psa")))
    println(sentencer(List("Alan","ma","dosc","!","tegoniema")))
    println(sentencer(List("Czy","Aleksandra","cos","jeszcze","ma","?")))

    println("zad.3");
    def graterZeroChecker(list: List[Int]): Boolean = {
      def graterZeroCheckerHlp(list: List[Int],notEmpty: Boolean): Boolean =
        if list == Nil then notEmpty
        else if list.head < 0 then false
        else graterZeroCheckerHlp(list.tail, true)
      graterZeroCheckerHlp(list,false)
    }

    println(graterZeroChecker(List()));
    println(graterZeroChecker(List(1,2,-3)));
    println(graterZeroChecker(List(2,1,1,5,0)));

    println("zad.4");
    def factorial(x:Int):Int =
      def factorialIter(x:Int,acc:Int):Int =
        if x == 0 then acc
        else if x<0 then -1
        else factorialIter(x-1,acc*x)
      factorialIter(x,1)

    println(factorial(6));
    println(factorial(0));
    println(factorial(-5));

  }
}
