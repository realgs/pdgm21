import scala.annotation.tailrec

object Main {
  def main(args: Array[String]): Unit = {

    println(convertTo16(31))
    println(convertNumber(31, 16))


  }
}



def convertTo16(number: Int) =
  @tailrec
  def converNumberHelper(value: Int, resultList: List[Int]): List[Int] =
    if(value > 0) then converNumberHelper((value / 16), (value % 16)::resultList)
    else resultList

  converNumberHelper(number,Nil)


def convertNumber(number: Int, base: Int) =
  @tailrec
  def converNumberHelper(value: Int, resultList: List[Int]): List[Int] =
    if(value > 0) then converNumberHelper((value / base), (value % base)::resultList)
    else resultList

  converNumberHelper(number,Nil)




