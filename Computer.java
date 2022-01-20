
import java.util.HashMap;
import java.util.Map;

public class Computer implements Player{

    @Override
    public int choosePit(int whichPlayer,Pit[] board){
        int opponent=0;
        if(whichPlayer==1){
            opponent=2;
        }else opponent=1;
        Map<Integer,Integer> maxes=new HashMap<>();
        for(int i=0;i<6;i++){
            Board newBoard=new Board(Board.copyBoard(board));
            if(whichPlayer==1 && newBoard.getBoard()[i].getStones()!=0 || whichPlayer==2 && newBoard.getBoard()[i+7].getStones()!=0){
                newBoard.move(i,whichPlayer);
                int max = Integer.MIN_VALUE;
                for(int j=0;j<6;j++){
                    Board insideBoard=new Board(Board.copyBoard(board));
                    if(opponent==1 && insideBoard.getBoard()[i].getStones()!=0 || opponent==2 && insideBoard.getBoard()[i+7].getStones()!=0){
                        insideBoard.move(j,opponent);
                        if(insideBoard.countDifference(opponent,insideBoard.getBoard()) >max){
                            max=insideBoard.countDifference(opponent,insideBoard.getBoard());
                        }
                    }
                }
                maxes.put(i,max);
            }

        }
        int min =Integer.MAX_VALUE;
        int bestChoice=-1;
        for(Map.Entry<Integer,Integer> entry: maxes.entrySet()){
            if(entry.getValue()<min){
                bestChoice=entry.getKey();
            }
        }
        System.out.println("computer chose: "+ bestChoice);
        return bestChoice;

    }


}
