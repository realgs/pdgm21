public class Pit {
    private int stones;
    private int whichPlayer;
    private int positionOnBoard;

    public Pit(int stones, int whichPlayer, int positionOnBoard){
        this.stones=stones;
        this.whichPlayer=whichPlayer;
        this.positionOnBoard=positionOnBoard;
    }

    public Pit(Pit newPit){
        this.stones=newPit.stones;
        this.whichPlayer=newPit.whichPlayer;
        this.positionOnBoard=newPit.positionOnBoard;
    }

    @Override
    public String toString() {
        return positionOnBoard+"("+stones+")";
    }

    public int getStones() {
        return stones;
    }

    public int getWhichPlayer() {
        return whichPlayer;
    }

    public int getPositionOnBoard() {
        return positionOnBoard;
    }

    public void setStones(int stones) {
        this.stones = stones;
    }

    public void setWhichPlayer(int whichTeam) {
        this.whichPlayer = whichPlayer;
    }

    public void setPositionOnBoard(int positionOnBoard) {
        this.positionOnBoard = positionOnBoard;
    }
}
