package kalaha.service

import kalaha.view.GuiCreator
import kalaha.view.panels._

import javax.swing.{JButton, JLabel, JTextField}

class Service(gui: GuiCreator):
	val connectionPanel: ConnectionPanel = gui.controlsPanel.connectionPanel
	val namePanel: NamePanel = gui.controlsPanel.namePanel
	val gamePanel: GamePanel = gui.controlsPanel.gamePanel
	val playersPanel: PlayersPanel = gui.controlsPanel.playersPanel
	val boardPanel: BoardPanel = gui.boardPanel
	
	val connectButton: JButton = connectionPanel.connectButton
	val showPlayersButton: JButton = playersPanel.showPlayersButton
	val nameSubmitButton: JButton = namePanel.nameSubmitButton
	val nameTextField: JTextField = namePanel.nameTextField
	val startGameButton: JButton = gamePanel.startGameButton
	
	val firstPlayerHoles: Array[JButton] = boardPanel.firstPlayerHoles
	val bases: Array[JLabel] = boardPanel.bases
	val secondPlayerHoles: Array[JButton] = boardPanel.secondPlayerHoles
	
	def onConnected: Unit =
		connectionPanel.connectStatus.setText("Connected!")
		connectionPanel.connectButton.setEnabled(false)
		
		nameTextField.setEnabled(true)
		nameSubmitButton.setEnabled(true)
		startGameButton.setEnabled(true)
		showPlayersButton.setEnabled(true)
		
	def onRegistered: Unit =
		nameTextField.setEnabled(false)
		nameSubmitButton.setEnabled(false)
		
	def onPlayers(name0: String, name1: String): Unit =
		playersPanel.firstPlayerLabel.setText(name0)
		playersPanel.secondPlayerLabel.setText(name1)
			
	def onGameStarted: Unit =
		startGameButton.setEnabled(false)
		nameTextField.setEnabled(false)
		nameSubmitButton.setEnabled(false)
		showPlayersButton.setEnabled(false)
		
	def onRemaining(remaining: String): Unit =
		gamePanel.timeoutLabel.setText(s"Time: $remaining")
		
	def onGameOver(winner: String): Unit =
		for b <- firstPlayerHoles do
			b.setEnabled(false)
			bases(0).setText(s"${bases(0).getText.toInt + b.getText.toInt}")
		for b <- secondPlayerHoles do
			b.setEnabled(false)
			bases(1).setText(s"${bases(1).getText.toInt + b.getText.toInt}")
		gamePanel.timeoutLabel.setText(s"Game over, winner: $winner")	