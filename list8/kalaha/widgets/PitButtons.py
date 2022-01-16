from qtpy.QtWidgets import QPushButton, QSizePolicy
from kalaha.stylesheets import *


class PitButton(QPushButton):

    def __init__(self, index, parent):
        super(PitButton, self).__init__(parent)
        self.index = index
        font = self.font()
        font.setPointSize(20)
        self.setFont(font)
        self.setStyleSheet(OPPONENT_PIT_STYLESHEET)
        sizepolicy = QSizePolicy(QSizePolicy.Minimum, QSizePolicy.Expanding)
        self.setSizePolicy(sizepolicy)


class BaseButton(PitButton):

    def __init__(self, index, parent):
        super(BaseButton, self).__init__(index, parent)
        self.setStyleSheet(OPPONENT_BASE_STYLESHEET)
        self.setMaximumWidth(300)
