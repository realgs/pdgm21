trait Debug {
    def debugName: String = getClass.getName
    def debugVars: List[List[String]] = getClass.getDeclaredFields.toList.map{
        f => 
            f.setAccessible(true)
            val res = List(f.getName, f.getType.toString, f.get(this).toString)
            f.setAccessible(false)
            res
    }
}

class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
}

object List6 {

    def eachNElement[A](llist: LazyList[A], period: Int, end: Int): LazyList[A] = 
        def filter(llist: LazyList[A], index: Int): LazyList[A] =
            if llist.isEmpty then LazyList()
            else if index >= end then LazyList()
            else if index % period == 0
                then llist.head #:: filter(llist.tail, index + 1)
            else filter(llist.tail, index + 1)
        filter(llist, 0)
    
    def lazyExecute(list1: LazyList[Int], list2: LazyList[Int], op: String): LazyList[Int] =
        if list1.isEmpty then list2
        else if list2.isEmpty then list1
        else op match
            case "+" => (list1.head + list2.head) #:: lazyExecute(list1.tail, list2.tail, op)
            case "-" => (list1.head - list2.head) #:: lazyExecute(list1.tail, list2.tail, op)
            case "*" => (list1.head * list2.head) #:: lazyExecute(list1.tail, list2.tail, op)
            case "/" => (list1.head / list2.head) #:: lazyExecute(list1.tail, list2.tail, op)
            case _ => list1.head #:: lazyExecute(list1.tail, list2.tail, op)

    def duplicate[A](values: LazyList[A], counts: LazyList[Int]): LazyList[A] =
        if values.isEmpty then LazyList()
        else if counts.isEmpty then LazyList()
        else if counts.head <= 0 then duplicate(values.tail, counts.tail)
        else values.head #:: duplicate(values, (counts.head - 1) #:: counts.tail)

    def main(args: Array[String]): Unit =
        // test eachNElement()
        println("test eachNElement()")
        println(eachNElement(LazyList(5, 6, 3, 2, 1), 2, 3).toList == List(5, 3))
        println(eachNElement(LazyList(5, 6, 3, 2, 1), 2, 4).toList == List(5, 3))
        println(eachNElement(LazyList(5, 6, 3, 2, 1), 2, 5).toList == List(5, 3, 1))
        println(eachNElement(LazyList(), 2, 5).toList == List())
        println(eachNElement(LazyList(5, 6), 1, 5).toList == List(5, 6))
        // test lazyExecute()
        println("test lazyExecute()")
        println(lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), "+").toList == List(3, 5, 7, 5))
        println(lazyExecute(LazyList(1, 2, 3), LazyList(3, 3, 3), "/").toList == List(0, 0, 1))
        println(lazyExecute(LazyList(0, 1, -1), LazyList(1, 1, 1), "-").toList == List(-1, 0, -2))
        println(lazyExecute(LazyList(), LazyList(), "").toList == List())
        // test duplicate()
        println("test duplicate()")
        println(duplicate(LazyList(1, 2, 3), LazyList(0, 3, 1, 4)).toList == List(2, 2, 2, 3))
        println(duplicate(LazyList(1, 2, 3), LazyList(0, 1)).toList == List(2))
        println(duplicate(LazyList(1, 2, 3), LazyList()).toList == List())
        println(duplicate(LazyList(), LazyList(0, 3, 1, 4)).toList == List())
        println(duplicate(LazyList(), LazyList()).toList == List())
        // test Debug
        println("test Debug")
        var p : Point = new Point(3, 4)
        println(p.debugName == "Point")
        var r : Point = new Point(4, 3)
        println(r.debugName == "Point")
        println(r.debugVars)
}