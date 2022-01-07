class Kalaha() {

  private val field1:Array[Int] = Array(6,6,6,6,6,6,0)
  private val field2:Array[Int] = Array(6,6,6,6,6,6,0)

  def move(id: Int, field: Int): Int ={

    var level = id == 1
    var tmpField = newField(level)
    var points = tmpField(field)
    tmpField(field) = 0
    var index = field
    while(points > 0 && index <= 6){
      index += 1
      if index == 7 then {
        index = 0
        level = !level
        tmpField = newField(level)
      }
      tmpField(index) += 1
      points -= 1
    }

    if (id==1) == level && index <= 5 then
      val otherField = newField(!level)
      if tmpField(index) == 1 then
        tmpField(6) += otherField(5 - index)
        otherField(5-index) = 0

    if (id==1) == level && index == 6 then
      if level then 1 else 2
    else
      if level then 2 else 1
  }

  def isOver:Boolean = {

    var isOver1 = false
    var isOver2 = false
    field1.foreach(x => isOver1 = x==0)
    field2.foreach(x => isOver2 = x==0)
    isOver1 || isOver2
  }

  def whoWon():Int = {

    var res = 0
    val res1 = field1.foldLeft(0)((x, sum)=> sum + x)
    val res2 = field2.foldLeft(0)((x, sum)=> sum + x)
    if res1 > res2 then res = 1
    else if res1 < res2 then res = 2
    res
  }

  val newField1: Int => Array[Int] = id => if id == 1 then field1 else field2

  val newField: Boolean => Array[Int] = value =>if value then field1 else field2

}
