package Kalaha.Server

import Kalaha.Gameboard.Gameboard
import Kalaha.User.{Computer_Random, Player, Player_human}
import concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future, TimeoutException}
import concurrent.duration.DurationInt

class Server {
  val time = 2000
  var gameboard = new Gameboard

  var player_move = 0
  var player1: Player = _
  var player2: Player = _


  def chose_players: Unit =
    println("Jaki rodzaj rozgrywki wybierasz")
    println("1 - Użytkownik vs Użytkownik")
    println("2 - Użytkownik vs Komputer")
    println("3 - Komputer vs Komputer")
    var move = 0

    while (move <= 0 || move > 3)
        move = scala.io.StdIn.readInt()


    move match
      case 1 =>
        player1 = new Player_human(gameboard)
        player2 = new Player_human(gameboard)

      case 2 =>
        player1 = new Player_human(gameboard)
        player2 = new Computer_Random(gameboard,this)
      case 3 =>
        player1 = new Computer_Random(gameboard,this)
        player2 = new Computer_Random(gameboard,this)



  def initialize_game(): Unit =

    chose_players
    gameboard.prepareBoard()

  def game(): Unit =
    initialize_game()

    while(gameboard.is_possible_move)
      try { val is_move = Future {
        gameboard.print_board()
        println("Teraz ruch gracza:" + player_move)
        var helper1 = true
        var move = 0
        while helper1 do
          move = if (player_move == 0) then player1.make_move() else player2.make_move()
          if !gameboard.is_legal_move(move, player_move) then println("Podano zły ruch :(") else helper1 = false

        if !gameboard.move(move, player_move) then player_move = (player_move + 1) % 2
        gameboard.print_winner()
      }

        Await.result(is_move,time.second)}
        catch {
          case e: TimeoutException=> println("Przekroczono limit czasu")
            return -1
        }}



