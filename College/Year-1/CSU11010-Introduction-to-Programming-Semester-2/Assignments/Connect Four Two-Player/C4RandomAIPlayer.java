import java.util.Random;

public abstract class C4RandomAIPlayer extends ConnectPlayer{

	static int compPlayerId;

	C4RandomAIPlayer(int playerId){
		super(playerId);
		this.compPlayerId = compPlayerId;
	}

	@Override
	public int playerMove(Connect4Grid2DArray connect4Grid) {
		Random randomObj = new Random();
		boolean col = false;
		int c = 0;
		c= (int)randomObj.nextInt(7);
		while(!col) {
			if(connect4Grid.isColumnFull(c)) {
				c = (int) randomObj.nextInt(7);
			}
			else {
				col = true;
			}
		}

		System.out.println("AI Move: Column " +(c));
		return c;
	}

	public static void main(String[] args) {
		// TODO Auto-generated method stub

	}

}
