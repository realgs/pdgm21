package model.serverModel

import model.GameSpecification
import model.serverModel.PlayerHandler.*

import java.awt.image.DataBuffer
import java.io.*
import java.net.Socket
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object PlayerHandler:
  val TIMEOUT: String = "TIME OUT"
  val TIMEGOOD: String = "TIME GOOD"

  private var players: Array[PlayerHandler] = Array()
  private var _maxTimeOut: Int= 120
  def maxTimeOut: Int = _maxTimeOut

  private var _time: Int = 0
  def time: Int = _time

  private var _isFirstPlayer: Boolean = true
  def isFirstPlayer: Boolean = _isFirstPlayer
  def isFirstPlayer_=(newVal: Boolean) = _isFirstPlayer = newVal

  private var isFull1: Boolean = false
  private var isFull2: Boolean = false

class PlayerHandler(_socket: Socket, name: String) extends Runnable:
  import PlayerHandler.*
  private val socket: Socket = _socket
  private val dataInput: DataInputStream = new DataInputStream(socket.getInputStream)
  private val dataOutput: DataOutputStream = new DataOutputStream(socket.getOutputStream)
  private var playerUsername: String = dataInput.readUTF()
  if players.length == 1 && players(0).playerUsername == playerUsername then playerUsername = playerUsername + "2"
  players = players :+ this
  dataOutput.writeUTF(playerUsername)
  dataOutput.write(if _isFirstPlayer then 1 else 0)
  println(GameSpecification.STARTAMOUNTOFROCKS)
  dataOutput.write(GameSpecification.STARTAMOUNTOFROCKS)
  dataOutput.flush()

  def measureTime(): Unit =
    Future{
      if _isFirstPlayer|| (isFull1 && isFull2) then
        while _time < _maxTimeOut + 2 do
          {Thread.sleep(1000); _time += 1}}

  def waitForSecondConnection(): Unit =
    while !socket.isClosed && _time < _maxTimeOut && (!isFull1 || !isFull2) do
      if players.length == 2 then
        if isFull1 then isFull2 = true
        isFull1 = true
        players.foreach(
          player =>
            if player.playerUsername != playerUsername then
            {
              dataOutput.writeUTF(TIMEGOOD)
              dataOutput.writeUTF(player.playerUsername )
              dataOutput.flush()
            }
        )

    if _time >= _maxTimeOut then
      dataOutput.writeUTF(TIMEOUT)
      dataOutput.writeUTF("PC")
      dataOutput.flush()
      close(this)

  def maintainSendReceive(): Unit =
    _time = 0
    _maxTimeOut = 30
    while !socket.isClosed && _time < _maxTimeOut && isFull2 do
      if dataInput.available() > 0 && isFull1 && isFull2 then
        _time = 0
        val moveIndex = dataInput.read()
        val isEnding = dataInput.read()
        val isNextMove = dataInput.read()
        val playerScore = dataInput.read()
        println(moveIndex + " " + isEnding + " " + isNextMove + "\n")
        sendToOtherPlayer(moveIndex, isEnding, isNextMove, playerScore)
        if isEnding == 1 then
          isFull2 = false
          players.foreach(elem => close(elem))


    if !socket.isClosed && _time >= _maxTimeOut then
      if isFull2 then
        dataOutput.writeUTF(TIMEOUT)
        dataOutput.write(7)
        dataOutput.write(1)
        dataOutput.write(1)
        dataOutput.write(0)
        dataOutput.flush()
      Thread.sleep(1000)
      close(this)

  override def run(): Unit =
    println(isFirstPlayer)
    measureTime()
    waitForSecondConnection()
    if !socket.isClosed then maintainSendReceive()



  def sendToOtherPlayer(moveIndex: Int, isEnding: Int, isNextMove: Int, playersScore: Int): Unit =
    players.foreach(
      player =>
        if player.playerUsername != playerUsername then
        {
          println(playerUsername)
          player.dataOutput.writeUTF(TIMEGOOD)
          player.dataOutput.write(moveIndex)
          player.dataOutput.write(isEnding)
          player.dataOutput.write(isNextMove)
          player.dataOutput.write(playersScore)
          player.dataOutput.flush()
        }
    )

  def removePlayer(playerHandler: PlayerHandler): Unit =
    players = players.filter(_ ne playerHandler)
    println(s"${playerHandler.playerUsername} has disconnected")

  def close(playerHandler: PlayerHandler): Unit =
    removePlayer(playerHandler)
    dataInput.close()
    dataOutput.close()
    socket.close()

