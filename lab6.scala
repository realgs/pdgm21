def eachNElement[A](elements: LazyList[A], n: Int, m: Int) =
    def eachNElementTailrec(elements: LazyList[A], i: Int, result: LazyList[A]): LazyList[A] =
        if i < m && elements != Nil then
            eachNElementTailrec(elements.tail, i+1, if i%n == 0 then elements.head #:: result else result)
        else result.reverse
    eachNElementTailrec(elements.tail, 1, LazyList[A](elements.head))

eachNElement(LazyList[Int](5, 6, 3, 2, 1), 2, 3) == LazyList(5, 3)
eachNElement(LazyList[Int](5, 6, 3, 2, 1), 2, 4) == LazyList(5, 3)

def lazyExecute(firstList: LazyList[Int], secondList: LazyList[Int], operator: ((Int, Int) => Int)) =
    def lazyExecuteTailrec(firstList: LazyList[Int], secondList: LazyList[Int], result: LazyList[Int]): LazyList[Int] =
        (firstList, secondList) match
            case (fh #:: ft, sh #:: st) => lazyExecuteTailrec(ft, st, operator(fh, sh) #:: result)
            case (LazyList(), secondList) => (secondList.reverse #::: result).reverse
            case (firstList, LazyList()) => (firstList.reverse #::: result).reverse
    lazyExecuteTailrec(firstList, secondList, LazyList())

val + = (a: Int, b: Int) => a + b

lazyExecute(LazyList[Int](1, 2, 3), LazyList[Int](2, 3, 4, 5), +) == LazyList(3, 5, 7, 5)

def repeatInCollection(list: List[Int], repeats: List[Int]) =
    def repeatInCollectionTailrec(list: List[Int], repeats: List[Int], result: List[Int]): List[Int] =
        (list, repeats) match
            case (lh :: lt, rh :: rt) =>
                if rh == 0 then repeatInCollectionTailrec(list.tail, repeats.tail, result)
                else repeatInCollectionTailrec(list, (rh - 1) :: rt, lh :: result)
            case (Nil, _) => result.reverse
            case (_, Nil) => result.reverse
    repeatInCollectionTailrec(list, repeats, Nil)

repeatInCollection(List(1, 2, 3), List(0, 3, 1, 4)) == List(2, 2, 2, 3)

trait Debug {
    def debugName() =
        this.getClass.getName

    def mapper(field: java.lang.reflect.Field) =
        val name = field.getName
        val typeName = field.getType
        val f = this.getClass.getDeclaredField(name)
        f.setAccessible(true)
        val value = f.get(this)
        List[Any](name, typeName, value)

    def debugVars() =
        this.getClass.getDeclaredFields.map(mapper)
}

class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
}

var p : Point = new Point(3, 4)
p.debugName()
p.debugVars()