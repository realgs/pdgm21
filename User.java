import java.util.Scanner;

public class User implements Player {
    @Override
    public int choosePit(int whichPlayer, Pit[] board) {
        int position;
        while (true) {
            Scanner scanner = new Scanner(System.in);
            System.out.print("choose position:");
            position = scanner.nextInt();
            if (whichPlayer == 1 && board[position].getStones() != 0 && position >= 0 && position < 6 ){
                return position;
            }
            if(whichPlayer == 2 && board[position+7].getStones() != 0 && position >= 0 && position < 6){
                return position;
            }
        }
    }

}
