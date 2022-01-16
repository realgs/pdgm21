package Kalaha.Gameboard

import Kalaha.User.Player

class Game_board {

  val NUMBER_HOUSES = 6
  val NUMBER_STONES = 5
  val BASE = 6
  var houses: Array[Array[Int]] = Array.ofDim[Int](2, NUMBER_HOUSES + 1)


  def cloneBoard(): Array[Array[Int]] = {
    Array(houses(0).clone, houses(1).clone)
  }

  def prepareBoard(): Unit =
    for (i <- 0 until 2)
      for (j <- 0 until NUMBER_HOUSES)
        houses(i)(j) = NUMBER_STONES

  def is_legal_move(number: Int, player: Int): Boolean =
    if number > NUMBER_HOUSES then false
    else if houses(player)(number) == 0 then false else true


  def move(house_number: Int, player: Int): Boolean =
    var next_move = false
    var index = house_number
    val number_stones = houses(player)(house_number)
    houses(player)(house_number) = 0
    var now_site = player
    val last = (house_number + number_stones) % (NUMBER_HOUSES + 1)
    for (i <- 1 to number_stones)
      index = (index + 1) % (NUMBER_HOUSES + 1)

      houses(now_site)(index) += 1
      if (i == number_stones && last == BASE) {

        next_move = true
      }
      else if (houses(player)(last) == 1 && i == number_stones && player == now_site) {
        houses(player)(BASE) = houses(player)(BASE) + 1 + houses(get_enemy_player(player))(last)
        houses(player)(index) = 0
        houses(get_enemy_player(player))(last) = 0
      }
      if index == BASE then now_site = get_enemy_player(now_site)

    next_move


  def calculate_point_diff(board: Array[Array[Int]], player: Int): Int =
    board(player)(BASE) - board((player + 1) % 2)(BASE)

  def get_enemy_player(player: Int): Int =
    (player + 1) % 2

  def is_possible_move: Boolean =
    var empty: (Int, Int) = (0, 0)

    for (i <- 0 until NUMBER_HOUSES)
      if houses(0)(i) == 0 then empty = (empty._1 + 1, empty._2)
      if houses(1)(i) == 0 then empty = (empty._1, empty._2 + 1)

    if empty._1 == NUMBER_HOUSES || empty._2 == NUMBER_HOUSES then false else true

  def print_board(): Unit =
    println(houses(0).toList)
    println(houses(1).toList)


  def print_winner(): Unit =
    var player_stones: (Int, Int) = (0, 0)
    for (i <- 0 until NUMBER_HOUSES + 1)
      player_stones = (player_stones._1 + houses(0)(i), player_stones._2 + houses(1)(i))
    println("Gracz 1 uzyskał: " + player_stones._1 + " punktów ")
    println("Gracz 2 uzyskał: " + player_stones._2 + " punktów ")
    if player_stones._1 > player_stones._2 then println("Gracz 1 jest zwycięzcą")
    if player_stones._1 < player_stones._2 then println("Gracz 2 jest zwycięzcą")
    if player_stones._1 == player_stones._2 then println("Remis")

  def board_after_theoretical_move(board: Array[Array[Int]], house_number: Int, player: Int): (Array[Array[Int]], Boolean) =
    val game = new Game_board
    game.houses = board
    val bonus = game.move(house_number, player)
    (game.houses, bonus)

  def valid_moves(player: Int): List[Int] =
    var list: List[Int] = List()
    for (i <- 0 until BASE)
      if is_legal_move(i, player) then list = i :: list

    list


}
