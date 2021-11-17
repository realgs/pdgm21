import scala.annotation.tailrec

object Lista4 {

    //reverse helping method
    def reverse[A](list: List[A]): List[A] = {
      @tailrec
      def reverse(list: List[A], reversed: List[A]): List[A] =
        list match
          case Nil => reversed
          case head :: tail => reverse(list.tail, head :: reversed)

      reverse(list, Nil)
    }

    //contains helping method
    def contains(string: String, key: String): Boolean = {
       (string, key) match
          case (_, "") => true
          case ("", _) => false
          case (string, key) => if string.head == key.head then contains(string.tail, key.tail)
                                else contains(string.tail, key)
    }

    //Zad1

    //single key

    def find[A](list1: List[String], sentance: String): List[String] = {
      list1 match
        case Nil => Nil
        case head :: tail =>
          if contains(head,sentance)==true then head::find(tail,sentance)
          else find(tail,sentance)
    }

    def findTail[A](list1: List[String], sentance: String): List[String] = {
      @tailrec
      def findTailHelper[A](list1: List[String], sentance: String,result: List[String]): List[String] = {
        list1 match
          case Nil => reverse(result)
          case head :: tail =>
            if contains(head,sentance)==true then findTailHelper(tail,sentance,head::result)
            else findTailHelper(tail,sentance,result)
      }
      findTailHelper(list1,sentance,Nil)
    }

    //list of keys

    def containsList(string: String, listOfKeys: List[String]): Boolean = {
      listOfKeys match
        case Nil => false
        case head :: tail => if contains(string,head) then true
                             else containsList(string,tail)
    }


    //recursive
    def findList(list1: List[String], listOfKeys: List[String]): List[String] = {
      if listOfKeys == Nil then list1
      else list1 match
        case head :: tail =>
          if containsList(head,listOfKeys)==true then head::findList(tail,listOfKeys)
          else findList(tail,listOfKeys)
        case Nil => Nil
    }

    //tail recursive
    def findListTail[A](list1: List[String], listOfKeys: List[String]): List[String] = {
      @tailrec
      def findListTailHelper[A](list1: List[String], listOfKeys: List[String],result: List[String]): List[String] = {
        if listOfKeys == Nil then list1
        else list1 match
            case Nil => reverse(result)
            case head :: tail =>
              if containsList(head,listOfKeys)==true then findListTailHelper(tail,listOfKeys,head::result)
              else findListTailHelper(tail,listOfKeys,result)
      }
      findListTailHelper(list1,listOfKeys,Nil)
    }


    //Zad2

    //tail recursive
    def joinListsTailRec[A](list1: List[A], list2: List[A], list3: List[A]): List[A] = {
      @tailrec
      def joinListsHelper[A](list1: List[A], list2: List[A], list3: List[A], listResult: List[A]): List[A] = {
        (list1, list2, list3) match
          case (head :: tail, list2, list3) => joinListsHelper(tail, list2, list3, head :: listResult)
          case (Nil, head :: tail, list3) => joinListsHelper(Nil, tail, list3, head :: listResult)
          case (Nil, Nil, head :: tail) => joinListsHelper(Nil, Nil, tail, head :: listResult)
          case (Nil, Nil, Nil) => reverse(listResult)
      }
      joinListsHelper(list1, list2, list3, Nil)
    }

    //recursive
    def joinListsRec[A](list1: List[A], list2: List[A], list3: List[A]): List[A] = {
      (list1, list2, list3) match
        case (head :: tail, list2, list3) => head :: joinListsRec(tail, list2, list3)
        case (Nil, head :: tail, list3) => head :: joinListsRec(Nil, tail, list3)
        case (Nil, Nil, head :: tail) => head :: joinListsRec(Nil, Nil, tail)
        case (Nil, Nil, Nil) => Nil
   }

    def main(args: Array[String]): Unit ={

      System.out.println(find(List("filip" ,"illipf", "plip", "lipfili","po","fpo"), "ili")==List("filip", "illipf", "lipfili"))
      System.out.println(findTail(List("filip" ,"illipf", "plip", "lipfili","po","fpo"), "ili")==List("filip", "illipf", "lipfili"))
      System.out.println(find(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), "index0168") == List("iindex0168202", "iindex0168211", "iindex0168210"))
      System.out.println(findTail(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), "index0168") == List("iindex0168202", "iindex0168211", "iindex0168210"))

      System.out.println(findList(List("filip" ,"illipf", "plip", "lipfili","po","fpo"), List("ili", "po"))==List("filip", "illipf", "lipfili", "po", "fpo"))
      System.out.println(findListTail(List("filip" ,"illipf", "plip", "lipfili","po","fpo"), List("ili", "po"))==List("filip", "illipf", "lipfili", "po", "fpo"))
      System.out.println(findList(List("123" ,"123456", "234", "34578","56","456"), List("56", "78"))==List("123456", "34578", "56", "456"))
      System.out.println(findListTail(List("123" ,"123456", "234", "34578","56","456"), List("56", "78"))==List("123456", "34578", "56", "456"))

      System.out.println(joinListsRec(List(1,2),List(3,4), List(5,6))==List(1, 2, 3, 4, 5, 6))
      System.out.println(joinListsTailRec(List(1,2),List(3,4), List(5,6))==List(1, 2, 3, 4, 5, 6))
      System.out.println(joinListsRec(List("Mario ","to "),List("klasyk "), List("gier ","video"))==List("Mario " , "to " , "klasyk " , "gier " , "video"))
      System.out.println(joinListsTailRec(List("Mario ","to "),List("klasyk "), List("gier ","video"))==List("Mario " , "to " , "klasyk " , "gier " , "video"))
    }
}

