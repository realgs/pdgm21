from qtpy.QtWidgets import QWidget, QVBoxLayout, QLabel
from qtpy.QtCore import Slot, Qt
from qrainbowstyle.widgets import WaitingSpinner


class StatusWidget(QWidget):
    def __init__(self, parent=None):
        super().__init__(parent)
        self.widget_layout = QVBoxLayout(self)
        self.widget_layout.setSpacing(100)
        self.setLayout(self.widget_layout)
        self.widget_layout.setAlignment(Qt.AlignVCenter)

        self.spinner = WaitingSpinner(self, centerOnParent=True,
                                      roundness=70.0,
                                      fade=70.0, radius=50.0, lines=12,
                                      line_length=20.0, line_width=5.0)
        self.widget_layout.addWidget(self.spinner)

        self.status_label = QLabel(self)
        font = self.font()
        font.setPointSize(15)
        self.status_label.setFont(font)
        self.status_label.setAlignment(Qt.AlignCenter)
        self.widget_layout.addWidget(self.status_label)
        self.spinner.start()

    @Slot(str)
    def set_status(self, status):
        self.status_label.setText(status)

    @Slot()
    def show(self):
        self.spinner.start()
        super().show()

    @Slot()
    def hide(self):
        self.spinner.stop()
        super().hide()
