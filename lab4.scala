import annotation.tailrec

object Main {
  def main(args: Array[String]): Unit = {
    println(find(List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"),"index0168"))
    println(find2(List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"),"22","index0168"))
    println(find3(List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"),"22","index0168"))
    println(find4(List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"),"22","index0168"))

  }
}


def find(list: List[String],value : String) =
  list.filter(x => x.contains(value))


def find2(list: List[String],values : String*) =
  list.filter(x => findInMultipleValues(x,values*))

def findInMultipleValues(word: String, values : String*):Boolean =
  if values == Nil then false
  else if word.contains(values.head) then true
  else findInMultipleValues(word,values.tail*)


def find3(list: List[String],values : String*):List[String] =
  list match {
    case Nil => Nil
    case (h::t) => if(findInMultipleValues(h,values*)) then h :: find3(t,values*) else find3(t,values*)
  }

def find4(list: List[String],values : String*) =
  @tailrec
  def find4Helper(innerList: List[String], result: List[String],values : String* ):List[String] =
    innerList match {
      case Nil => result
      case h::t=> if(findInMultipleValues(h,values*)) then find4Helper(t,h::result,values*) else find4Helper(t,result,values*)
    }
  find4Helper(list,Nil,values*)

