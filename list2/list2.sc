/*
Zsumuj wartość pierwszego elementu następnie rekurencyjnie wywołuj sum na ogonie listy.
*/
def sum(list: List[Int]): Int =
    if list == Nil then 0
    else list.head + sum(list.tail)

println(sum(List(5, 1, 12, -5, 0)))
println(sum(List()))
println(sum(Nil))


// stała hardkodowana
val SPACING = " "

def join(list: List[String], end: String): String =
    if list == Nil then ""
    else
        if list.tail == Nil then list.head + end
        else list.head + SPACING + join(list.tail, end)

join(List("To", "nie", "tak"), "!")
join(List("Ala", "ma", "kota"), "?")
join(List(), "wow!")
join(Nil, "some string")

/*
Funkcja ma zwrócić True tylko gdy wszystkie liczby z listy są większe od 0.
W każdym innym przypadku False (również w przypadku pustej listy i Nil'a).
*/
def greater_than_zero(list: List[Double]): Boolean =
    if list == Nil then false
    else
        // bez tego warunku funckja zwróci False dla 'dobrej' listy
        if list.tail == Nil then list.head > 0
        else list.head > 0 && greater_than_zero(list.tail)

greater_than_zero(List(1, -5, 3, 2))
greater_than_zero(List(1, 2, 5, 7, 8))
greater_than_zero(List())
greater_than_zero(Nil)


def factorial (n: Int): Int =
    if n < 0 then 0
    else if n == 0 then 1 else n * factorial(n - 1)

factorial(-5)
factorial(0)
factorial(6)
