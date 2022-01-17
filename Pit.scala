sealed trait Pit:
  var seeds: Int
  def increment(): Unit = this.seeds += 1
  def add(amount: Int): Unit = this.seeds += amount
  def clear(): Int =
    val t = this.seeds
    this.seeds = 0
    t
case class House(var seeds: Int, ownerId: Int, index: Int) extends Pit
case class Store(var seeds: Int, ownerId: Int) extends Pit
