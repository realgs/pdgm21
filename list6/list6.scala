import java.lang.reflect.Field
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.LinkedHashMap

object Lista6 {
    /*
    Basically Stream was only lazy on its tail, so the head was always computed.
    For many people this was surprising and lead to erroneous code.
    On the other hand LayzList is fully lazy. It doesn't compute the head until it is called.
    Note that when you compute the head of a LayzList also computes its tail and viceversa.

    Source https://stackoverflow.com/questions/60128207/whats-the-difference-between-lazylist-and-stream-in-scala
    */

    //Zadanie
    def eachNElement[A](list: LazyList[A], step: Int, last: Int): LazyList[A] =
        def eachNElementHelper(list: LazyList[A], index: Int): LazyList[A] =
            if (index == last) LazyList()
            else if (index % step == 0) list.head #:: eachNElementHelper(list.tail, index + 1)
            else eachNElementHelper(list.tail, index + 1)
        if step < 1 then throw new IllegalArgumentException("'step' argument must be greater than 0")
        if last < 0 then throw new IllegalArgumentException("'last' argument must be greater or equal 0")
        eachNElementHelper(list, 0)

    //Zadanie 2
    def lExecute(list1: LazyList[Double], list2: LazyList[Double], operator: Char): LazyList[Double] =
        def lExecuteHelper(operation: (Double, Double) => Double, l1: LazyList[Double], l2: LazyList[Double]): LazyList[Double] =
            (l1, l2) match
                case (h1 #:: t1, h2 #:: t2) => operation(h1, h2) #:: lExecuteHelper(operation, t1, t2)
                case (LazyList(), _) => l2
                case (_, LazyList()) => l1
        operator match
            case '+' => lExecuteHelper((x: Double, y: Double) => x + y, list1, list2)
            case '-' => lExecuteHelper((x: Double, y: Double) => x - y, list1, list2)
            case '/' => lExecuteHelper((x: Double, y: Double) => x / y, list1, list2)
            case '*' => lExecuteHelper((x: Double, y: Double) => x * y, list1, list2)
            case _ => throw new NotImplementedError("unsupported operation: '" + operator + "'")



    //Zadanie 3
    def duplicate[A](elements: List[A], count: List[Int]): List[A] =
        // Given a list of elements and a list of counts, duplicate the elements
        // the number of times specified by the counts.
        def mapElementsToCounts(elements: List[A], count: List[Int], dictionary: LinkedHashMap[A, Int]): LinkedHashMap[A, Int] = {
            if elements.isEmpty || count.isEmpty then dictionary
            else
                if dictionary.contains(elements.head) then
                    if dictionary.last._1 == elements.head then
                        dictionary.update(dictionary.last._1, dictionary.last._2 + count.head)
                    else ()
                else
                    dictionary.put(elements.head, count.head)
                mapElementsToCounts(elements.tail, count.tail, dictionary)
        }

        def linkedHashMapToList(dictionary: LinkedHashMap[A, Int], result: List[A]): List[A] = {
            // Given a dictionary where values are the number of times to duplicate the key return a list
            if dictionary.isEmpty then result
            else
                if dictionary.head._2 > 0 then
                    // dictionary.update(dictionary.head._1, dictionary.head._2 - 1)
                    // `updated` is deprecated, use `addOne` instead
                    linkedHashMapToList(dictionary.addOne(dictionary.head._1, dictionary.head._2 - 1), dictionary.head._1 :: result)
                else
                    linkedHashMapToList(dictionary.tail, result)
        }
        val hashMap = mapElementsToCounts(elements, count, LinkedHashMap())
        linkedHashMapToList(hashMap, List()).reverse

    trait Debug {
        // Zadanie 4
        def debugName(): String =
            getClass.getName

        // Zadanie 5
        def vars(): LinkedHashMap[String, Any] = {
            val objectVars = LinkedHashMap[String, Any]()
            for(field <- getClass.getDeclaredFields) {
                field.setAccessible(true)
                objectVars.put(field.getName, field.get(this))
                field.setAccessible(false)
            }
            objectVars
        }

        def debugVars(): String = {
            val buffer = ListBuffer[String]()
            for (field <- vars()) {
                buffer += "[" + (Array(field._1, field._2.getClass, field._2.toString)).mkString(", ") + "]"
            }
            "["+ (buffer.toList.toArray).mkString(", ") +"]"
        }

        def fields(): LinkedHashMap[Field, Any] = {
            val objectVars = LinkedHashMap[Field, Any]()
            for(field <- getClass.getDeclaredFields) {
                field.setAccessible(true)
                objectVars.put(field, field.get(this))
                field.setAccessible(false)
            }
            objectVars
        }

        def debugFields(): String = {
            val buffer = ListBuffer[String]()
            for (field <- fields()) {
                buffer += "[" + (Array(field._1.getName.toString, field._1.getType.toString, field._2.toString)).mkString(", ") + "]"
            }
            "["+ (buffer.toList.toArray).mkString(", ") +"]"
        }
    }

    class Point(xv: Int, yv: Int) extends Debug {
        var x: Int = xv
        var y: Int = yv
        var a: String = "test"
    }

    def main(args: Array[String]): Unit = {
        val llist = LazyList(5, 6, 3, 2, 1)
        println("Zadanie 1: ")
        println(eachNElement(llist, 2, 3).force)
        println(eachNElement(llist, 2, 4).force)
        println(eachNElement(llist, 1, 5).force)
        println(eachNElement(llist, 6, 2).force)
        println(eachNElement(llist, 6, 0).force)

        println("Zadanie 2: ")
        println(lExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '+').force)
        println(lExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '-').force)
        println(lExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '*').force)
        println(lExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '/').force)

        println("Zadanie 3: ")
        //[/1;2;3] oraz [0;3;1;4] daje wynik [2;2;2;3]
        println(duplicate(List(1, 2, 3), List(0, 3, 1, 4)) == List(2, 2, 2, 3))
        println(duplicate(List(1, 2, 3, 3, 5, 3), List(1, 2, 2, 2, 3, 3)) == List(1, 2, 2, 3, 3, 3, 3, 5, 5, 5))

        var p : Point = new Point(3, 4);
        println("Zadanie 4: ")
        println(p.debugName())

        println("Zadanie 5: ")
        println(p.vars())
        println(p.debugVars())

        println(p.fields())
        println(p.debugFields())
    }

}

