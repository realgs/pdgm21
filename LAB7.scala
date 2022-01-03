import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

/*
4 watkowy procesor

Dodawanie takich samych tablic 2D - console outputs
[10x10] Time without paraller:      3450200
[10x10] Time with    paraller:      101225600

[100x100] Time without paraller:    908400
[100x100] Time with    paraller:    2858500

[1000x1000] Time without paraller:  17228900
[1000x1000] Time with    paraller:  9732100

[5000x5000] Time without paraller:  378542700
[5000x5000] Time with paraller:     227512300

[10000x10000] Time without paraller:515787600
[10000x10000] Time with paraller:   314302100

Intuicyjne wnioski:
- Programowanie równoległe ma sens gdy mamy do czynienia z problemem który niesie ze sobą duże ilości danych,
 które mogą być w równoległy sposób przetworzone jak i na koniec połączone.
- Nie powinniśmy zrównoleglać prostego zadania (z małą ilością danych) które
jest w stanie wykonać jeden wątek, może się okazać że podział obowiązków zajmie o wiele dłużej niż samo policzenie problemu(zad2).
- Trzeba również zwracać uwagę na ewentualna ilosc wywołań rekurencyjnych(pamięć).

Podsumowując - zrównoleglanie to bardzo przydatna technika poprawy wyniku czasowego algorytmu,
zawsze warto sprawdzić jak zachowa się zrównoleglanie w przypadku naszego algorytmu. Być może dzięki temu będziemy mogli
efektywniej operować ilością wątków w zależnosci od naszych rozmiarów danych,
co przyśpieszy ogólne działania programu.

Zad 2. sumowanie tablicy 1D
[4] Time without paraller: 604000
[4] Time with paraller:    2523600

[40] Time without paraller: 14900
[40] Time with paraller:    91600

[400] Time without paraller: 55500
[400] Time with paraller:    104000

[4000] Time without paraller: 55900
[4000] Time with paraller:    365600

[40000] Time without paraller: 526100
[40000] Time with paraller:    929000

[400000] Time without paraller: 4439500
[400000] Time with paraller:    2477300

[4000000] Time without paraller: 54192000
[4000000] Time with paraller:    37240300

[40000000] Time without paraller: 444765100
[40000000] Time with paraller:    235087300

*/


object LAB7 {

  def createMatrix(n: Int, m: Int): Array[Array[Int]] = {
    var matrix = Array.ofDim[Int](n, m)

    for (i <- 0 to n - 1)
      for (j <- 0 to m - 1) {
        matrix(i)(j) = 1
      }

    return matrix
  }

  def add2MatrixDefault(matrix1: Array[Array[Int]], matrix2: Array[Array[Int]]): Int = {
    var sum = 0

    for (i <- 0 to matrix1.length - 1)
      for ( j <- 0 to matrix1(0).asInstanceOf[Array[Int]].length - 1)
        sum += matrix1(i)(j) + matrix2(i)(j)

    return sum
  }

  def add2MatrixParell(matrix1: Array[Array[Int]], matrix2: Array[Array[Int]]): Int = {
    def add2MatrixParellInside(matrix1: Array[Array[Int]], matrix2: Array[Array[Int]],rowStart: Int, rowEnd: Int) : Int = {
      var sum = 0

      for(i <- rowStart to rowEnd - 1)
        for (j <- 0 to matrix1(0).asInstanceOf[Array[Int]].length - 1)
          sum += matrix1(i)(j) + matrix2(i)(j)

      return sum
    }


    val f1 = Future{add2MatrixParellInside(matrix1,matrix2,0,matrix1.length / 4)}
    val f2 = Future{add2MatrixParellInside(matrix1,matrix2,matrix1.length / 4, matrix1.length / 2)}
    val f3 = Future{add2MatrixParellInside(matrix1,matrix2,matrix1.length / 2, matrix1.length * 3 / 4)}
    val f4 = Future{add2MatrixParellInside(matrix1,matrix2,matrix1.length * 3 / 4, matrix1.length)}

    return Await.result(f1, Duration.Inf) + Await.result(f2, Duration.Inf) + Await.result(f3, Duration.Inf) +
      Await.result(f4, Duration.Inf)
  }


  ///////////////////////////////////////////////////////////////////////////////////////////////////////////

  def createArray(length: Int): Array[Int] = {
    var arr = Array.ofDim[Int](length)

    for (i <- 0 to length-1) {
      arr(i) = 1
    }

    return arr
  }

  def sum1DDefault(arr: Array[Int]): Int = {
    var sum = 0

    for (i <- 0 to arr.length-1) {
      sum += arr(i)
    }

    return sum
  }

  def sum1DParaller(arr: Array[Int]): Int = {
    def sum1DParallerInside(arr: Array[Int], rowStart: Int, rowEnd: Int): Int = {
      var sum = 0
      for (i <- rowStart to rowEnd - 1) {
        sum += arr(i)
      }
      return sum
    }

    val f1 = Future{sum1DParallerInside(arr, 0, arr.length / 4)}
    val f2 = Future{sum1DParallerInside(arr, arr.length / 4, arr.length / 2)}
    val f3 = Future{sum1DParallerInside(arr, arr.length / 2, arr.length * 3 / 4)}
    val f4 = Future{sum1DParallerInside(arr, arr.length * 3 / 4, arr.length)}


    return Await.result(f1, Duration.Inf) + Await.result(f2, Duration.Inf) + Await.result(f3, Duration.Inf) +
      Await.result(f4, Duration.Inf)
  }




  def main(args: Array[String]): Unit = {
    val tm1 = createMatrix(10, 10)
    val tm2 = createMatrix(10, 10)

    var time = System.nanoTime()
    val ans12 = add2MatrixDefault(tm1,tm2)
    println("[10x10] Time without paraller:      " + (System.nanoTime() - time).toString)

    time = System.nanoTime()
    val ans12_2 = add2MatrixParell(tm1,tm2)
    println("[10x10] Time with    paraller:      " + (System.nanoTime() - time).toString)


    val tm3 = createMatrix(100, 100)
    val tm4 = createMatrix(100, 100)

    time = System.nanoTime()
    val ans34 = add2MatrixDefault(tm3,tm4)
    println("[100x100] Time without paraller:    " + (System.nanoTime() - time).toString)

    time = System.nanoTime()
    val ans34_2 = add2MatrixParell(tm3,tm4)
    println("[100x100] Time with    paraller:    " + (System.nanoTime() - time).toString)

    val tm5 = createMatrix(1000, 1000)
    val tm6 = createMatrix(1000, 1000)

    time = System.nanoTime()
    val ans56 = add2MatrixDefault(tm5,tm6)
    println("[1000x1000] Time without paraller:  " + (System.nanoTime() - time).toString)

    time = System.nanoTime()
    val ans56_2 = add2MatrixParell(tm5,tm6)
    println("[1000x1000] Time with    paraller:  " + (System.nanoTime() - time).toString)

    val tm7 = createMatrix(5000, 5000)
    val tm8 = createMatrix(5000, 5000)

    time = System.nanoTime()
    val ans78 = add2MatrixDefault(tm7,tm8)
    println("[5000x5000] Time without paraller:  " + (System.nanoTime() - time).toString)

    time = System.nanoTime()
    val ans78_2 = add2MatrixParell(tm7,tm8)
    println("[5000x5000] Time with paraller:     " + (System.nanoTime() - time).toString)

    val tm9 = createMatrix(10000, 10000)
    val tm10 = createMatrix(10000, 10000)

    time = System.nanoTime()
    val ans910 = add2MatrixDefault(tm9,tm10)
    println("[10000x10000] Time without paraller:" + (System.nanoTime() - time).toString)

    time = System.nanoTime()
    val ans910_2 = add2MatrixParell(tm9,tm10)
    println("[10000x10000] Time with paraller:   " + (System.nanoTime() - time).toString)

    ////////////////////////////////////////////////////////////////////////////////////

    val a1 = createArray(4)

    time = System.nanoTime()
    val ans1 = sum1DDefault(a1)
    println("[4] Time without paraller: " + (System.nanoTime() - time).toString)

    time = System.nanoTime()
    val ans2 = sum1DParaller(a1)
    println("[4] Time with paraller: " + (System.nanoTime() - time).toString)

    val a2 = createArray(40)

    time = System.nanoTime()
    val ans3 = sum1DDefault(a2)
    println("[40] Time without paraller: " + (System.nanoTime() - time).toString)

    time = System.nanoTime()
    val ans4 = sum1DParaller(a2)
    println("[40] Time with paraller: " + (System.nanoTime() - time).toString)

    val a3 = createArray(400)

    time = System.nanoTime()
    val ans5 = sum1DDefault(a3)
    println("[400] Time without paraller: " + (System.nanoTime() - time).toString)

    time = System.nanoTime()
    val ans6 = sum1DParaller(a3)
    println("[400] Time with paraller: " + (System.nanoTime() - time).toString)

    val a4 = createArray(4000)

    time = System.nanoTime()
    val ans7 = sum1DDefault(a4)
    println("[4000] Time without paraller: " + (System.nanoTime() - time).toString)

    time = System.nanoTime()
    val ans8 = sum1DParaller(a4)
    println("[4000] Time with paraller: " + (System.nanoTime() - time).toString)

    val a5 = createArray(40000)

    time = System.nanoTime()
    val ans9 = sum1DDefault(a5)
    println("[40000] Time without paraller: " + (System.nanoTime() - time).toString)

    time = System.nanoTime()
    val ans10 = sum1DParaller(a5)
    println("[40000] Time with paraller: " + (System.nanoTime() - time).toString)

    val a6 = createArray(400000)

    time = System.nanoTime()
    val ans11 = sum1DDefault(a6)
    println("[400000] Time without paraller: " + (System.nanoTime() - time).toString)

    time = System.nanoTime()
    val ans122 = sum1DParaller(a6)
    println("[400000] Time with paraller: " + (System.nanoTime() - time).toString)

    val a7 = createArray(4000000)

    time = System.nanoTime()
    val ans13 = sum1DDefault(a7)
    println("[4000000] Time without paraller: " + (System.nanoTime() - time).toString)

    time = System.nanoTime()
    val ans14 = sum1DParaller(a7)
    println("[4000000] Time with paraller: " + (System.nanoTime() - time).toString)

    val a8 = createArray(40000000)

    time = System.nanoTime()
    val ans15 = sum1DDefault(a8)
    println("[40000000] Time without paraller: " + (System.nanoTime() - time).toString)

    time = System.nanoTime()
    val ans16 = sum1DParaller(a8)
    println("[40000000] Time with paraller: " + (System.nanoTime() - time).toString)


  }

}
