package Kalah;

import java.util.Scanner;

public class Human implements Player {
    @Override
    public int chooseAMove(int team, Hole[] actualBoard) {
        int number = -1;
        while (true) {
            Scanner scanner = new Scanner(System.in);
            System.out.print("choose field:");
            number = scanner.nextInt();
            if (number < 6 && number >= 0 && team == 1 && actualBoard[number].getStones() != 0) {
                return number;
            }
            if (number < 6 && number >= 0 && team == 2 && actualBoard[number + 7].getStones() != 0) {
                return number;
            }
            System.out.println("An invalid number was given or the field is empty!");
        }
    }

}

