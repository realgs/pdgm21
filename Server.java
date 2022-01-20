import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;

public class Server {
    Board board;
    Boolean disconnected=false;

    public Server(Board board) {
        this.board = board;
    }

    public void gameplay(Player player1, Player player2) {
        boolean nextMove = true;
        while (true) {
            while (nextMove) {
                board.printBoard();
                if (board.checkIfEnd()) {
                    return;
                }
                final int[] move = new int[1];
                System.out.print("P1 ");
                CompletableFuture<Void> future = CompletableFuture.runAsync(() -> move[0] = player1.choosePit(1, Board.copyBoard(board.getBoard())));
                try {
                    future.get(30, TimeUnit.SECONDS);
                } catch (Exception e) {
                    future.cancel(true);
                    disconnected = true;
                    return;
                }
                System.out.println(board.move(move[0], 1));
                nextMove = board.move(move[0], 1);

            }
            nextMove = true;
            while (nextMove) {
                board.printBoard();
                if (board.checkIfEnd()) {
                    return;
                }
                final int[] move = new int[1];
                System.out.print("P2 ");
                CompletableFuture<Void> future = CompletableFuture.runAsync(() -> move[0] = player2.choosePit(2, Board.copyBoard(board.getBoard())));
                try {
                    future.get(30, TimeUnit.SECONDS);
                } catch (Exception e) {
                    future.cancel(true);
                    disconnected = true;
                    return;
                }
                System.out.println(board.move(move[0], 2));
                nextMove = board.move(move[0], 2);
            }
            nextMove = true;


        }
    }

    public void run(Player player1,Player player2)  {
        gameplay(player1,player2);
        if(disconnected==true){
            System.out.println("Rozłączono z serwerem!");
        }
        else {
            board.printResults();
        }
    }

}
