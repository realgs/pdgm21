package Kalah;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;

public class Server {
    Board board;

    public Server(Board board) {
        this.board = board;
    }


    public void game(Player player1, Player player2) {
        StupidComputer bot = new StupidComputer();
        boolean anotherMove = true;
        while (true) {
            while (anotherMove) {
                board.showBoard();
                if (board.isOver()) {
                    return;
                }
                final int[] move = new int[1];
                System.out.print("PLAYER 1 ");
                CompletableFuture<Void> future = CompletableFuture.runAsync(() -> move[0] = player1.chooseAMove(1, Board.copyBoard(board.getBoard())));
                try {
                    future.get(60, TimeUnit.SECONDS);
                } catch (Exception e) {
                    // Operation timed out
                    future.cancel(true);
                    System.out.println("no data\nWe lost connection with player 1 \nBot will take over his role!");
                    try {
                        Thread.sleep(10000);
                    } catch (InterruptedException ex) {
                        ex.printStackTrace();
                    }
                    move[0] = bot.chooseAMove(1, Board.copyBoard(board.getBoard()));
                }
                anotherMove = board.moveStones(move[0], 1);
            }
            anotherMove = true;
            while (anotherMove) {
                board.showBoard();
                if (board.isOver()) {
                    return;
                }
                final int[] move = new int[1];
                System.out.print("PLAYER 2 ");
                CompletableFuture<Void> future = CompletableFuture.runAsync(() -> move[0] = player2.chooseAMove(2, Board.copyBoard(board.getBoard())));
                try {
                    future.get(60, TimeUnit.SECONDS);
                } catch (Exception e) {
                    // Operation timed out
                    future.cancel(true);
                    System.out.println("no data\nWe lost connection with player 2 \nBot will take over his role!");
                    try {
                        Thread.sleep(10000);
                    } catch (InterruptedException ex) {
                        ex.printStackTrace();
                    }
                    move[0] = bot.chooseAMove(2, Board.copyBoard(board.getBoard()));
                }
                anotherMove = board.moveStones(move[0], 2);
            }
            anotherMove = true;


        }
    }

    public void play(Player player1, Player player2) {
        game(player1, player2);
        board.showBoard();
        System.out.println("\n\n\nGAME OVER");
        if (board.whoWon() == 0) {
            System.out.println("Draw! ");
        } else {
            System.out.println("Player numer " + board.whoWon() + " is the winner!");
        }
    }
}

