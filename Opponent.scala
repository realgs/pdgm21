package Kalah

abstract class Opponent(s_name:String, i: Int) {
  private val name=s_name
  private val index=i
  def move(board: Board):Int

  override def toString: String = s_name
}
