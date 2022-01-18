package Server

import Player.Computer.Computer
import Player.Human.Human

import java.io.{BufferedReader, BufferedWriter, InputStreamReader, OutputStreamWriter}
import java.net.Socket
import java.io.IOException
import java.util.Scanner
import scala.concurrent.*
import ExecutionContext.Implicits.global

object Client {
  class Client(var socket: Socket, val username: String) {
    var br: BufferedReader = _
    var bw: BufferedWriter = _

    try {
      bw = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream()))
      br = new BufferedReader(new InputStreamReader(socket.getInputStream()))
    } catch case _: IOException => closeEverything(socket, br, bw)

    def sendMessage(): Unit = {
      try {
        bw.write(username)
        bw.newLine()
        bw.flush()
        val scanner = Scanner(System.in)
        while (socket.isConnected()) {
          val messageToSend = scanner.nextLine()
          bw.write(messageToSend)
          bw.newLine()
          bw.flush()
        }
      } catch case _: IOException => closeEverything(socket, br, bw)
    }

    def listenForMessage(): Unit = {
      Future {
        var msgFromOthers = ""
        while (socket.isConnected()) {
          try {
            msgFromOthers = br.readLine()
            println(msgFromOthers)
          } catch case _: IOException => closeEverything(socket, br, bw)
        }
      }
    }

    def closeEverything(socket: Socket, bufferedReader: BufferedReader, bufferedWriter: BufferedWriter): Unit = {
      try {
        if bufferedReader != null then bufferedReader.close()
        if bufferedWriter != null then bufferedWriter.close()
        if socket != null then socket.close()
      } catch case e: IOException => e.printStackTrace()
    }
  }

  def main(args: Array[String]): Unit = {
    val scanner = Scanner(System.in)
    print("Enter your username: ")
    val username = scanner.nextLine()
    val socket = new Socket("localhost", 1234)
    val client = new Client(socket, username)
    client.listenForMessage()
    client.sendMessage()
  }
  
}
