package io.github.reconsolidated;

import java.util.Scanner;

public class Main {

    private static Scanner in = new Scanner(System.in);

    public static void main(String[] args) {
        String input = "";
        while (!input.equalsIgnoreCase("2")) {
            System.out.println("Press 1 to play pvp.");
            System.out.println("Press 2 quit");

            input = in.nextLine();
            if (input.equalsIgnoreCase("1")) {
                playerVsPlayer();
            }
        }
    }

    private static void playerVsPlayer() {
        GameClient client1 = new GameClient();
        GameClient client2 = new GameClient();
        GameServer server = new GameServer();
        client1.connect(server);
        client2.connect(server);

        server.startGame();

        String input = "";
        while (!input.equalsIgnoreCase("exit")) {
            if (!server.hasStarted()) {
                System.out.println("Press anything to start the game.");

            } else {
                System.out.println("Press 1 to control client 1.");
                System.out.println("Press 2 to control client 2.");
                System.out.println("Press 3 to control server.");
                System.out.println("Type exit to quit.");
                input = in.nextLine();
                if (input.equalsIgnoreCase("1")) {
                    if (server.getTurn() == 0) {
                        System.out.println("It's your turn. Type the hole you want to use (1-6): ");
                        input = in.nextLine();
                        int hole = -1;
                        try {
                            hole = Integer.parseInt(input);
                        } catch (NumberFormatException e) {
                            System.out.println("That's not a valid number.");
                        }
                        client1.move(hole);
                    } else {
                        System.out.println("Wait for your turn... ");
                    }
                }
                else if (input.equalsIgnoreCase("2")) {
                    if (server.getTurn() == 1) {
                        System.out.println("It's your turn. Type the hole you want to use (1-6): ");
                        input = in.nextLine();
                        int hole = -1;
                        try {
                            hole = Integer.parseInt(input);
                        } catch (NumberFormatException e) {
                            System.out.println("That's not a valid number.");
                        }
                        client2.move(hole);
                    } else {
                        System.out.println("Wait for your turn... ");
                    }
                }
                else if (input.equalsIgnoreCase("3")) {
                    System.out.println(client1.getState());
                }
            }
            server.run();
        }
    }
}
