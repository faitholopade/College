/* SELF ASSESSMENT
Connect4Game class (35 marks)
My class creates references to the Connect 4 Grid and two Connect 4 Players. It asks the user whether he/she would like to play/quit inside a loop. If the user decides to play then: 1. Connect4Grid2DArray is created using the Connect4Grid interface, 2. the two players are initialised - must specify the type to be ConnectPlayer, and 3. the game starts. In the game, I ask the user where he/she would like to drop the piece. I perform checks by calling methods in the Connect4Grid interface. Finally a check is performed to determine a win. 
Comment: Yes my class does what is specified above

Connect4Grid interface (10 marks)
I define all 7 methods within this interface.
Comment: Yes I define all 7 methods in the interface 

Connect4Grid2DArray class (25 marks) 
My class implements the Connect4Grid interface. It creates a grid using a 2D array Implementation of the method to check whether the column to drop the piece is valid. It provides as implementation of the method to check whether the column to drop the piece is full. It provides as implementation of the method to drop the piece.  It provides as implementation of the method to check whether there is a win.
Comment: Yes my class does what is required above 

ConnectPlayer abstract class (10 marks)
My class provides at lest one non-abstract method and at least one abstract method. 
Comment: Yes my class meets this requirement 

C4HumanPlayer class (10 marks)
My class extends the ConnectPlayer claas and overrides the abstract method(s). It provides the Human player functionality.
Comment: Yes my class does what is needed 

C4RandomAIPlayer class (10 marks)
My class extends the ConnectPlayer claas and overrides the abstract method(s). It provides AI player functionality. 
Comment:Yes my class extends connectPlayer and overrides the abstract methods and provides ai functionaly using the random function

Total Marks out of 100:100
 */
import java.util.Scanner;

public class Connect4Game {

	public static void main(String[] args) {
		System.out.println("Welcome to Connect 4: GOOD LUCK");
		Scanner inputScanner = new Scanner(System.in);

		Connect4Grid2DArray board = new Connect4Grid2DArray();

		System.out.print("Player 1 Type (ai, human): ");
		ConnectPlayer player1 = inputScanner.nextLine().equals("ai") ? new C4RandomAIPlayer((int)1) : new C4HumanPlayer((int)1, inputScanner);

		System.out.print("Player 2 Type (ai, human): ");
		ConnectPlayer player2 = inputScanner.nextLine().equals("ai") ? new C4RandomAIPlayer((int)2) : new C4HumanPlayer((int)2, inputScanner);

		boolean quit = false;

		int turn = 0;

		do {
			System.out.print("Would you like to continue playing, quit or clear the board? (play, quit, clear) : ");
			String userInput = inputScanner.nextLine();

			if (userInput.equalsIgnoreCase("quit")) {
				quit = true;
			} else {
				if (userInput.equalsIgnoreCase("clear")) {
					turn = 0;
					board.emptyGrid();
				}

				System.out.println("Turn " + turn);
				System.out.println("Determining moves for players..");

				board.dropPiece(player1, player1.playerMove(board));
				System.out.println(board);

				board.dropPiece(player2, player2.playerMove(board));
				System.out.println(board);

				if (board.didLastPieceConnect4()) {
					System.out.println("Player " + board.getWinner() + " has won!");
					return;
				}

				if (board.isGridFull()) {
					System.out.println("It was a tie. The board is full");
					return;
				}

				turn++;
			}

		} while (!quit);

		inputScanner.close();
	}
}