package view

import javax.swing.{JButton, JPanel, JTextField}

class ControlsPanel extends JPanel:
    var connectButton: JButton = new JButton("Connect to server")
    var nameTextField: JTextField = new JTextField("Your name")
    var nameSubmitButton: JButton = new JButton("Add player")
    var showPlayersButton: JButton = new JButton("Show players")
  
    add(connectButton)
    add(nameTextField)
    add(nameSubmitButton)
    add(showPlayersButton)
