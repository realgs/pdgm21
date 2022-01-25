package kalaha.view.panels

import kalaha.connection.Connection

import java.awt.{Color, FlowLayout}
import javax.swing.border.LineBorder
import javax.swing.{JButton, JPanel, JTextField}

class NamePanel extends JPanel:
    var nameTextField: JTextField = new JTextField("Your name")
    var nameSubmitButton: JButton = new JButton("Add player")

    nameTextField.setEnabled(false)
    nameSubmitButton.setEnabled(false)
    
    add(nameTextField)
    add(nameSubmitButton)
    setLayout(new FlowLayout(FlowLayout.CENTER, 10, 10))