import scala.util.control.Breaks.break

class Kalaha(private val field1: Array[Int], private val field2: Array[Int]) {

  def this() = {

    this(Array(6,6,6,6,6,6,0),Array(6,6,6,6,6,6,0))

  }

  def move(id: Int, field: Int): Int ={

    var level = id == 1
    var tmpField = nextField(level)
    var points = tmpField(field)
    tmpField(field) = 0
    var index = field
    while(points > 0 && index <= 6){
      index += 1
      if index == 7 then {
        index = 0
        level = !level
        tmpField = nextField(level)
      }
      tmpField(index) += 1
      points -= 1
    }

    if (id == 1) == level && index <= 5 then
      val otherField = nextField(!level)
      if tmpField(index) == 1 then
        tmpField(6) += otherField(5 - index)
        otherField(5-index) = 0

    if index == 6 then
      if (id == 1) == level then
        if level then 1 else 2
      else
        if level then 1 else 2
    else
      if id == 1 then 2 else 1
  }

  def getScore(id: Int): Int =
    if id == 1 then field1(6) - field2(6)
    else field2(6) - field1(6)

  def isOver():Boolean = {

    var isOver1 = true
    var isOver2 = true
    var i = 0
    while(i < 6){
      if field1(i) != 0 then isOver1 = false
      i += 1
    }
    i = 0
    while(i < 6){
      if field2(i) != 0 then isOver2 = false
      i += 1
    }
    isOver1 || isOver2
  }

  def whoWon():Int = {

    var res = 0
    val res1 = field1.foldLeft(0)((x, sum) => sum + x)
    val res2 = field2.foldLeft(0)((x, sum) => sum + x)
    if res1 > res2 then res = 1
    else if res1 < res2 then res = 2
    res
  }

  def printFields(id: Int): Unit = {

    var playerField = field1
    var enemyField = field2.reverse

    if id == 2 then
      playerField = field2
      enemyField = field1.reverse

    println("|---------------Enemy Field---------------|")
    println("|Base |--6--|--5--|--4--|--3--|--2--|--1--|")
    enemyField.foreach(x => printf("|%5d",x))
    println("|")
    println("|-----------------------------------------|")
    playerField.foreach(x => printf("|%5d",x))
    println("|")
    println("|--1--|--2--|--3--|--4--|--5--|--6--| Base|")
    println("|--------------Player Field---------------|")
  }

  def copy(): Kalaha = new Kalaha(field1.clone(), field2.clone())

  def getField(id: Int): Array[Int] =
    if id == 1 then field1
    else field2

  //val nextField1: Int => Array[Int] = id => if id == 1 then field1 else field2

  val nextField: Boolean => Array[Int] = value =>if value then field1 else field2

}
