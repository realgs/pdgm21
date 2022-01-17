abstract class Client (val name:String, var isFirst:Boolean){
  def isFirstPlayer: Boolean = isFirst
  def getName: String = name
  def chooseField(kalah: Kalah): Int
  def setIsFirst(value:Boolean): Unit

}
