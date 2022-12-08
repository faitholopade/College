
public abstract class Connect4Grid2DArray implements Connect4Grid {
	public static final int BOARD_WIDTH = 7;
	public static final int BOARD_HEIGHT = 6;

	public int[][] grid;
	private int winner;

	Connect4Grid2DArray(){
		grid = new int [BOARD_WIDTH][BOARD_HEIGHT];
	}

	public String toString() {
		StringBuilder builder = new StringBuilder()
				.append(" 0 1 2 3 4 5 6\n")
				.append("-----------------\n");

		for (int i = 0; i < BOARD_HEIGHT; i++) {
			builder.append('|');

			for (int j = 0; j < BOARD_WIDTH; j++) {
				builder.append(grid[i][j]);
				builder.append('|');
			}

			builder.append('\n');
		}

		builder.append("-----------------\n")
		.append(" 0 1 2 3 4 5 6\n");

		return builder.toString();
	}

	public static void main(String[] args) {
		// TODO Auto-generated method stub

	}

	@Override
	public void emptyGrid() 
	{
		for(int row = 0; row < BOARD_HEIGHT; row++){

			for(int column = 0; column < BOARD_WIDTH; column++){

				grid[row][column] = 0;
			}
		}

	}

	@Override
	public boolean isValidColumn(int column) 
	{
		if (column >= 0 && column < BOARD_WIDTH) {

			if (!isColumnFull(column))
				return true;
		}

		return false;
	}

	@Override
	public boolean isColumnFull(int column) 
	{

		if (column > BOARD_WIDTH-1) {
			return false;
		}

		for (int row = 0; row < BOARD_HEIGHT; row++) { 

			if (grid[column][row] == 0) 
			{
				return false;
			}
		}

		return true;
	}

	@Override
	public void dropPiece(ConnectPlayer player, int column) {

		if(isGridFull()) {
			return;
		}

		else if(!isValidColumn(column)) {
			return;
		}

		boolean dropped = false;

		for(int row = 0; row < BOARD_HEIGHT; row++) {

			if(grid[column][row] == 0 && row < BOARD_HEIGHT) {
				grid[column][row] = player.getPlayerId();
				return;
			}

			else if (row == BOARD_HEIGHT-1) {
				grid[column][row] = player.getPlayerId();
				return;
			}
		}
	}

	@Override
	public boolean didLastPieceConnect4() {

		// Check against Y axis (length)
		for(int i = 0; i < BOARD_HEIGHT ; i++) {
			for (int j = 0; j < BOARD_WIDTH - 3; j++) {
				// Checks if player 1 won
				if (grid[i][j] == 1 && grid[i][j+1] == 1 && grid[i][j+2] == 1 && grid[i][j+3] == 1) {
					winner = 1;
					return true;
				}

				// Checks if player 2 won
				if (grid[i][j] == 2 && grid[i][j+1] == 2 && grid[i][j+2] == 2 && grid[i][j+3] == 2) {
					winner = 2;
					return true;
				}
			}
		}

		// Check against X axis (width)
		for(int i = 0; i < BOARD_HEIGHT - 3; i++) {
			for (int j = 0; j < BOARD_WIDTH; j++) {
				// Checks if player 1 won
				if (grid[i][j] == 1 && grid[i+1][j] == 1 && grid[i+2][j] == 1 && grid[i+3][j] == 1) {
					winner = 1;
					return true;
				}

				// Checks if player 2 won
				if (grid[i][j] == 2 && grid[i+1][j] == 2 && grid[i+2][j] == 2 && grid[i+3][j] == 2) {
					winner = 2;
					return true;
				}
			}
		}

		// Check against Y/X (diagonal)
		for(int i = 0; i < BOARD_HEIGHT - 3; i++) {
			for(int j = 0; j < BOARD_WIDTH - 3; j++) {
				// Checks if player 1 won
				if (grid[i][j] == 1 && grid[i+1][j+1] == 1 && grid[i+2][j+2] == 1 && grid[i+3][j+3] == 1) {
					winner = 1;
					return true;
				}

				// Checks if player 2 won
				if (grid[i][j] == 2 && grid[i+1][j+1] == 2 && grid[i+2][j+2] == 2 && grid[i+3][j+3] == 2) {
					winner = 2;
					return true;
				}
			}
		}

		return false;
	}


	@Override
	public boolean isGridFull() {

		for(int row = 0; row < BOARD_HEIGHT; row++) {

			for(int column = 0; column < BOARD_WIDTH; column++) {

				if(grid[column][row] == 0) {
					return false;					
				}

			}
		}
		return true;
	}	

	public int getWinner() {
		return winner;
	}
}


