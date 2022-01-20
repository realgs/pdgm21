package Kalah;

public class Computer implements Player {

    @Override
    public int chooseAMove(int team, Hole[] actualBoard) {
        return Engine.calculateBestResult(actualBoard, 8, team, team);
    }

}

