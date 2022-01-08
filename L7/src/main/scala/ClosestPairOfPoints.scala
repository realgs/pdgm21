import scala.annotation.tailrec
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

class Point(xs : Int, ys: Int) {
  var x: Int = xs
  var y: Int = ys

}

object ClosestPairOfPoints {

  object xOrdering extends Ordering[Point] {
    override def compare(p1: Point, p2: Point): Int = {
      p1.x.compareTo(p2.x)
    }
  }

  object yOrdering extends Ordering[Point] {
    override def compare(p1: Point, p2: Point): Int = {
      p1.y.compareTo(p2.y)
    }
  }

  def countTime[A](task : => A) : Long = {
    val time = System.nanoTime()
    task
    System.nanoTime() - time
  }

  // COMMON METHODS

  def distance(p1 : Point, p2 : Point) : Double = {
    Math.sqrt((p1.x - p2.x) * (p1.x - p2.x) + (p1.y - p2.y) * (p1.y - p2.y))
  }

  def bruteForce(pointList : List[Point], count : Int) : Double = {
    var result = Double.MaxValue
    for ( i <- 0 until count) {
      for( j <- (i+1) until count) {
        result = Math.min(result, distance(pointList(i), pointList(j)))
      }
    }
    result
  }


  def initLists(points : List[Point]) : (List[Point], List[Point]) = {
    var xList : Array[Point] = Array()
    var yList : Array[Point] = Array()

    xList = xList.appendedAll(points)
    yList = yList.appendedAll(points)

    scala.util.Sorting.quickSort(xList)(xOrdering)
    scala.util.Sorting.quickSort(yList)(yOrdering)

    (xList.toList, yList.toList)
  }

  def initDividedLists(xList : List[Point], listLength : Int, mid : Int, midPoint : Point) : (List[Point], List[Point]) = {

    var newLowerXList: List[Point] = List()
    var newHigherXList: List[Point] = List()

    for (i <- 0 to mid) {
      newLowerXList = newLowerXList.appended(xList(i))
    }
    newLowerXList = newLowerXList.reverse

    for (i <- mid until listLength) {
      newHigherXList = newHigherXList.appended(xList(i))
    }
    newHigherXList = newHigherXList.reverse

    (newLowerXList, newHigherXList)
  }

  def initStripList(yList : List[Point], listLength : Int, midPoint : Point, minDistance : Double) : List[Point] = {
    var stripList: List[Point] = List()
    for (i <- 0 until listLength) {
      if Math.abs(yList(i).x - midPoint.x) < minDistance then {
        stripList = yList(i) :: stripList
      }
    }
    stripList = stripList.reverse
    stripList
  }

  // SEQUENTIAL

  def calculateStrip(stripList : List[Point], size : Int, minDistance : Double) : Double = {
    var minValue = minDistance
    for( i <- 0 until size) {
      for ( j <- (i+1) until Math.min(size, i + 1 + 7)) {
        minValue = Math.min(minValue, distance(stripList(i), stripList(j)))
      }
    }
    minValue
  }

  def closest(xList : List[Point], yList : List[Point], listLength : Int) : Double = {
    if listLength <= 3 then bruteForce(xList, xList.length)
    else {
      val mid = listLength / 2
      val midPoint = xList(mid)
      val (newLowerXList, newHigherXList) = initDividedLists(xList, listLength, mid, midPoint)

      val distanceLeft = closest(newLowerXList, yList, mid)
      val distanceRight = closest(newHigherXList, yList, listLength - mid)

      val minDistance = Math.min(distanceLeft, distanceRight)

      val stripList = initStripList(yList, listLength, midPoint, minDistance)

      Math.min(minDistance, calculateStrip(stripList, stripList.length, minDistance))
    }
  }

  def closestDistance(points : List[Point]) : Double = {

    val (xList, yList) = initLists(points)
    closest(xList, yList, xList.length)
  }

  // PARALLEL

  def calculateStripParallel(stripList : List[Point], size : Int, minDistance : Double) : Double = {
    var minValue = minDistance
    var minValues = for( i <- 0 until size) yield Future {
      var temp = minValue
      for ( j <- (i+1) until Math.min(size, i + 1 + 7)) {
        temp = Math.min(temp, distance(stripList(i), stripList(j)))
      }
      temp
    }
    minValues.map(Await.result(_, Duration.Inf)).min
  }

  def closestParallel(xList : List[Point], yList : List[Point], listLength : Int) : Double = {
    if listLength <= 3 then bruteForce(xList, xList.length)
    else {
      val mid = listLength / 2
      val midPoint = xList(mid)
      val (newLowerXList, newHigherXList) = initDividedLists(xList, listLength, mid, midPoint)

      val distanceLeft = Future {
        closest(newLowerXList, yList, mid)
      }
      val distanceRight = Future {
        closest(newHigherXList, yList, listLength - mid)
      }

      val minDistance = Math.min(Await.result(distanceLeft, Duration.Inf), Await.result(distanceRight, Duration.Inf))

      val stripList = initStripList(yList, listLength, midPoint, minDistance)

      Math.min(minDistance, calculateStripParallel(stripList, stripList.length, minDistance))
    }
  }

  def closestDistanceParallel(points : List[Point]) : Double = {
    val (xList, yList) = initLists(points)
    closest(xList, yList, xList.length)
  }

  def generateListOfPoints(n : Int, bound : Int) : List[Point] = {
    val random = scala.util.Random
    @tailrec
    def generateListHelper(list : List[Point], counter : Int) : List[Point] = {
      if counter == n then list
      else generateListHelper(Point(random.nextInt(bound), random.nextInt(bound)) :: list, counter + 1)
    }
    generateListHelper(List(), 0)
  }

  def process(points : List[Point]) : Unit = {
    println("-------------------")
    println("Length: " + points.length)
    print("SEQUENTIAL:   ")
    println(countTime(closestDistance(points)))
    print("PARALLEL:     ")
    println(countTime(closestDistanceParallel(points)))
    println("")
  }

  def proccessWithBruteForce(points : List[Point]) : Unit = {
    println("-------------------")
    println("Length: " + points.length)
    print("BRUTE FORCE:  ")
    println(countTime(bruteForce(points, points.length)))
    print("SEQUENTIAL:   ")
    println(countTime(closestDistance(points)))
    print("PARALLEL:     ")
    println(countTime(closestDistanceParallel(points)))
    println("")
  }

  def main(args: Array[String]) : Unit = {
    val points = List(Point(2, 3), Point(12, 30), Point(40, 50), Point(12, 10), Point(3, 4))
    val points2 = List(
      Point(77, 0),
      Point(1000, 1000),
      Point(992, 500),
      Point(10000000, 1),
    )
    proccessWithBruteForce(points)
    proccessWithBruteForce(generateListOfPoints(20, 1000))
    proccessWithBruteForce(generateListOfPoints(50, 10000))
    proccessWithBruteForce(generateListOfPoints(100, 100000))
    proccessWithBruteForce(generateListOfPoints(1000, 1000000))
    process(generateListOfPoints(5000, 1000000))
    //process(generateListOfPoints(50000, 10000000))
    /*
    For 50_000 points:
    SEQUENTIAL:   21573948373
    PARALLEL:     15933866767
    */
    println(closestDistance(points))
    println(closestDistanceParallel(points))
    println(closestDistance(points2))
    println(closestDistanceParallel(points2))
  }
}
