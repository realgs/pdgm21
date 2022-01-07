package view.panels

import controller.KalahaClientConnection

import java.awt.{Color, FlowLayout}
import javax.swing.border.LineBorder
import javax.swing.{JButton, JPanel, JTextField}

class NamePanel extends JPanel:
    var nameTextField: JTextField = new JTextField("Your name")
    var nameSubmitButton: JButton = new JButton("Add player")

    add(nameTextField)
    add(nameSubmitButton)
    setLayout(new FlowLayout(FlowLayout.CENTER, 10, 10))
    setBorder(new LineBorder(Color.ORANGE, 3))