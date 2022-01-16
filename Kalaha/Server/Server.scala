package Kalaha.Server

import Kalaha.Gameboard.Game_board
import Kalaha.User.{Computer_Pro, Computer_Random, Player, Player_human}

import concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future, TimeoutException}
import concurrent.duration.DurationInt

class Server {
  val time = 10
  var game_board = new Game_board

  var player_move = 0
  var player1: Player = _
  var player2: Player = _


  def chose_players(): Unit =
    println("Jaki rodzaj rozgrywki wybierasz")
    println("1 - Użytkownik vs Użytkownik")
    println("2 - Użytkownik vs Komputer")
    println("3 - Komputer vs Komputer")
    var move = 0

    while (move <= 0 || move > 3)
      move = scala.io.StdIn.readInt()


    move match
      case 1 =>
        player1 = new Player_human(game_board)
        player2 = new Player_human(game_board)

      case 2 =>
        player1 = new Player_human(game_board)
        player2 = new Computer_Pro(game_board, this)
      case 3 =>
        player1 = new Computer_Pro(game_board, this)
        player2 = new Computer_Random(game_board, this)


  def initialize_game(): Unit =

    chose_players()
    game_board.prepareBoard()

  def game(): Unit =
    initialize_game()

    while (game_board.is_possible_move)
      try {
        val is_move = Future {
          game_board.print_board()
          println("Teraz ruch gracza:" + player_move)
          var helper1 = true
          var move = 0
          while helper1 do
            move = if player_move == 0 then player1.make_move() else player2.make_move()
            if !game_board.is_legal_move(move, player_move) then print("Zły ruch :(") else helper1 = false

          println("Gracz " + player_move + " ruszył kamień o indeksie " + move)
          if !game_board.move(move, player_move) then player_move = (player_move + 1) % 2

        }

        Await.result(is_move, time.second)
      }
      catch
        case _: TimeoutException => println("Przekroczono limit czasu")
          return -1

    game_board.print_winner()
}




