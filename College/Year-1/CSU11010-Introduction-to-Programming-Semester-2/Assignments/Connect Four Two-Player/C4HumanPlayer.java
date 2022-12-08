import java.util.Scanner;

public abstract class C4HumanPlayer extends ConnectPlayer {

	Scanner scanner = new Scanner (System.in);
	static String PlayerId;
	public int column;

	C4HumanPlayer(int playerId, Scanner scanner){
		super(playerId);
		this.scanner= scanner;
	}

	@Override
	public int playerMove(Connect4Grid2DArray connect4Grid) {
		boolean col = false;
		while(!col) {
			System.out.println("Please pick a column to move your piece to");
			if(scanner.hasNextInt()) {
				column = scanner.nextInt();
			}
			else {
				System.out.println("Error: enter an integer value");
			}

			if(connect4Grid.isValidColumn(column - 1 ) && !connect4Grid.isColumnFull(column - 1)) {
				col = true;
			}
			else {
				System.out.println("Error: not a valid column, Please try again");
			}
		}
		return column - 1;
	}


}
