import scala.io.StdIn._

class KalahaUserInterface:

    val conn: KalahaClientConnection = new KalahaClientConnection

    def printMenu =
        println(" ===== KALAHA MENU ====== ")
        println("1) Connect to server")
        println("2) Show players")
        println("3) Start game")
        println("4) Make move")
        println("5) Quit")

    def mainLoop =
        var answer = 0
        while answer != 5 do
            printMenu
            try
                answer = readInt()
                answer match
                    case 1 =>
                        println("Enter your name:")
                        val name = readLine()
                        conn.name = name
                        try
                            conn.connectToServer
                        catch
                            case e: java.net.ConnectException => println("Something went wrong while trying to connect: " + e.getMessage)
                            case e => println(e.getMessage)
                    case 2 =>
                        try
                            conn.getPlayers
                        catch
                            case e: IllegalStateException => println(e.getMessage)
                    case 3 =>
                        try
                            conn.startGame
                        catch
                            case e: IllegalStateException => println(e.getMessage)
                    case 4 =>
                        println("Enter hole number:")
                        val holeNumber = readInt()
                        try
                            conn.makeMove(holeNumber)
                        catch
                            case e => println(e.getMessage)
                    case 5 =>
                        println("Quittnig!")
                        conn.disconnect
                    case _ =>
                        println("Incorrect value, try again!")
            catch
                case e: java.lang.NumberFormatException => println("Enter valid number, please!")
