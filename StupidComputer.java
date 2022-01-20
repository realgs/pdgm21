package Kalah;

public class StupidComputer implements Player {
    @Override
    public int chooseAMove(int team, Hole[] actualBoard) {

        int number = -1;
        while (true) {
            number = Engine.calculateRandomResult(actualBoard, team);
            if (number < 6 && number >= 0 && team == 1 && actualBoard[number].getStones() != 0) {
                return number;
            }
            if (number < 6 && number >= 0 && team == 2 && actualBoard[number + 7].getStones() != 0) {
                return number;
            }
        }

    }
}

