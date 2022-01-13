import ParallelResearchUtils.*

object BestSolution {
  //wyszukiwanie najlepszego rozwiązania w populacji
  // tablica zawierająca ciągi binarne jest przeszukiwana w celu znalezienia osobnika o najlepszym przystosowaniu (najwięcej 1)
  // rozwiązanie zrównoleglone jest lepsze dla populacji większych niż 10 000 osobników, próg zrównoleglenia 1000
  // wyniki dla powyższych danych: algorytm rónwoległy dwa razy lepszy (156ms do 328ms)


  def generateSeriesArray(size: Int ): Array[String] = {
    var counter = 0
    var array = Array[String]()
    while(counter < size){
      array = array :+ generateBinSeries(10000)
      counter += 1
    }
    array
  }

  def generateBinSeries(length: Int): String ={
    var series = ""
    var count = 0
    val rand = scala.util.Random()
    while( count < length){
      if rand.nextDouble() < 0.5 then series = series + "0"
      else series = series + "1"
      count += 1
    }
    series
  }

  def quality(series: String): Int ={
    var index = 0
    var quality = 0
    while(index < series.length){
      if(series.charAt(index) == '1') then quality += 1
      index += 1
    }
    quality
  }
  def findBestSolutionHelper(wholePopulation: Array[String], start: Int, end: Int): String ={
    if(end - start == 1) then
      if quality(wholePopulation(start)) < quality(wholePopulation(end)) then wholePopulation(end)
      else wholePopulation(start)
    else
      val partition = (start + end )/2
      val f1 = findBestSolutionHelper(wholePopulation, start, partition)
      val f2 = findBestSolutionHelper(wholePopulation, partition, end)
      if(quality(f1) < quality(f2))  then f2
      else f1
  }
  def findBestSolution(population: Array[String]): String ={
    findBestSolutionHelper(population, 0, population.size - 1)
  }

  def findBestSolutionParallel(population: Array[String]): String ={
    def findBestSolutionHelperParallel(wholePopulation: Array[String], start: Int, end: Int): String ={
      if(end - start < 1000) then findBestSolutionHelper(wholePopulation, start, end)
      else
        val partition = (start + end )/2
        val (r1, r2) = parallel(findBestSolutionHelperParallel(wholePopulation, start, partition), findBestSolutionHelperParallel(wholePopulation, partition, end))
        if(quality(r1) < quality(r2))  then r2
        else r1
    }
    findBestSolutionHelperParallel(population, 0, population.size - 1)
  }
}
