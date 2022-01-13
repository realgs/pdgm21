import ParallelResearchUtils.*

object QuickSort {
  // Czas sortowania zrównoleglonego był niższy tylko dla bardzo dużej liczby danych wejściowych ok.700 000, próg zrównoleglenia 300 000,
  // przykładowe wyniki dla 800000 danych: równoległy: 78ms, nierównoległy 94 ms
  // dla mniejszych wartości zdecydowana przewaga jednowątkowego rozwiązania

  def generateArray(length: Int): Array[Int] ={
    var array = Array[Int]()
    val rand = scala.util.Random()
    var currentLength = 0
    while(currentLength < length){
      array = array :+ rand.nextInt()
      currentLength += 1
    }
    array
  }

  def swap(tab: Array[Int])(i: Int)(j: Int) : Unit =
    var aux = tab(i)
    tab(i) = tab(j)
    tab(j) = aux

  def choose_pivot(tab: Array[Int])(m: Int)(n: Int): Int = tab((m + n)/2)

  def partition(tab: Array[Int])(l: Int)(r: Int): (Int, Int) =
    var i = l
    var j = r
    var pivot = choose_pivot(tab)(l)(r)
    while i <= j do
      while tab(i) < pivot do i += 1;
      while pivot < tab(j) do j -= 1;
      if i <= j then
        swap(tab)(i)(j);
        i += 1;
        j -= 1;
    (i, j)


  def quick(tab: Array[Int])(l: Int)(r: Int): Unit =
    if l < r then
      var (i, j) = partition(tab)(l)(r)
      if j - l < r - i then
        quick(tab)(l)(j)
        quick(tab)(i)(r)
      else
        quick(tab)(i)(r)
        quick(tab)(l)(j)

  def quicksort(tab: Array[Int]): Unit =
    quick(tab)(0)(tab.length - 1)

  def quickParallel(tab: Array[Int])(l: Int)(r: Int): Unit =
    if l < r then
      if r-l < 300000 then quick(tab)(l)(r)
      else
        var (i, j) = partition(tab)(l)(r)
        if j - l < r - i then
          parallel(quickParallel(tab)(l)(j), quickParallel(tab)(i)(r))
        else
          parallel(quickParallel(tab)(i)(r), quickParallel(tab)(l)(j))

  def quicksortParallel(tab: Array[Int]): Unit =
    quickParallel(tab)(0)(tab.length - 1)
}
