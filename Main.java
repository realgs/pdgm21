package Kalah;

import java.util.Scanner;

public class Main {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        while (true) {
            System.out.println("KALAH GAME by Filip Brzeziak\n");
            System.out.println("ATTENTION!\nAfter 60s of inactivity, bot will take your place!\n");
            System.out.println("Select a game mode");
            System.out.println("\tPLAYER 1\tPLAYER 2");
            System.out.println("1. Computer\tvs\tComputer");
            System.out.println("2. Computer\tvs\tHuman");
            System.out.println("3. Human\tvs\tComputer");
            System.out.println("4. Human\tvs\tHuman");
            System.out.print("Choice:");
            Server server = new Server(new Board());
            int choice = scanner.nextInt();
            switch (choice) {
                case 1: {
                    server.play(new Computer(), new Computer());
                    break;
                }
                case 2: {
                    server.play(new Computer(), new Human());
                    break;
                }
                case 3: {
                    server.play(new Human(), new Computer());
                    break;
                }
                case 4: {
                    server.play(new Human(), new Human());
                    break;
                }
            }
            System.out.print("If you want to exit press any number, if you want to play again press 0:");
            int numer = scanner.nextInt();
            if (numer != 0) {
                return;
            }
        }
    }
}

