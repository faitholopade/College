/* SELF ASSESSMENT 

1. ResolveBet
I have correctly defined ResolveBet which takes the bet type (String) and the Wallet object, and a void return type [Mark out of 7: 7].
Comment:I have correctly defined ResolveBet which takes the bet type (String) and the Wallet object, and a void return type
My program presents the amount of cash in the wallet and asks the user how much he/she would like to bet [Mark out of 8: 8].
Comment:My program presents the amount of cash in the wallet and asks the user how much he/she would like to bet
My program ensures the bet amount is not greater than the cash in the wallet [Mark out of 5: 5].
Comment:My program ensures the bet amount is not greater than the cash in the wallet
My program creates three Dice objects, rolls them and creates a total variable with a summation of the roll values returned [Mark out of 15: 15]..
Comment:My program creates three Dice objects, rolls them and creates a total variable with a summation of the roll values returned
My program determines the winnings by comparing the bet type with the total and comparing the bet type with the dice faces for the triple bet [Mark out of 20: 20 ].
Comment:My program determines the winnings by comparing the bet type with the total and comparing the bet type with the dice faces for the triple bet 
My program outputs the results (win or loss) and adds the winnings to the wallet if user wins or removes the bet amount from the wallet if the user loses [Mark out of 10: 10].
Comment: My program outputs the results (win or loss) and adds the winnings to the wallet if user wins or removes the bet amount from the wallet if the user loses

2. Main
I ask the user for the amount of cash he/she has, create a Wallet object and put this cash into it [Mark out of 15:15 ]
Comment: I ask the user for the amount of cash he/she has, create a Wallet object and put this cash into it
My program loops continuously until the user either enters quit or the cash in the wallet is 0 [Mark out of 5: 5]
Comment:My program loops continuously until the user either enters quit or the cash in the wallet is 0
I ask the user to enter any of the four bet types or quit [Mark out of 5: 5].
Comment: I ask the user to enter any of the four bet types or quit
My program calls resolveBet for each bet type entered [Mark out of 5:5 ].
Comment:My program calls resolveBet for each bet type entered
At the end of the game my program presents a summary message regarding winnings and losses [Mark out of 5:5 ]
Comment:At the end of the game my program presents a summary message regarding winnings and losses 

 Total Mark out of 100 (Add all the previous marks):100
*/
import java.util.Scanner;

public class ChuckaLuckDiceGame {

	public static void resolveBet(String betType, Wallet playerCash)
	{
		int playerBet = 0;
		System.out.println(playerCash.toString());
		System.out.print("How much would you like to bet? ");
		Scanner inputScanner = new Scanner(System.in);

		if(inputScanner.hasNextInt())
		{
			playerBet = inputScanner.nextInt();
		}

		else
		{
			System.out.println("Error enter a valid bet");
		}

		if (playerBet <= playerCash.check()) 
		{

			Dice diceone = new Dice();
			Dice dicetwo = new Dice();
			Dice dicethree = new Dice();

			int rollOne = diceone.roll();
			int rollTwo = dicetwo.roll();
			int rollThree = dicethree.roll();
			int total = rollOne + rollTwo + rollThree;
			boolean triple = false;

			if(rollOne != 1 && rollOne != 6 && rollOne == rollTwo && rollOne == rollThree)
			{
				triple = true;
			}

			playerCash.get(playerBet);

			if(betType.equals("Triple") && triple) 
			{
				System.out.println("Congratulations you won");
				playerCash.put( (playerBet * 31) );
			}

			else if(betType.equals("Field") && total < 8 || total > 12) 
			{
				System.out.println("Congratulations you won");
				playerCash.put( (playerBet * 2) );
			}

			else if
			(betType.equals("High") && total > 10 && !triple) 
			{
				System.out.println("Congratulations you won");				 
				playerCash.put( (playerBet * 2) );	
			}

			else if(betType.equals("Low") && total < 11 && !triple) 
			{
				System.out.println("Congratulations you won");
				playerCash.put( (playerBet * 2) );
			}

			else 
			{
				System.out.println("Commiserations you lost");
				playerCash.put((-playerBet));
			}

		} 
		else 
		{
			System.out.println("Insufficient funds please try again");
		}
	}
	public static void main(String[] args) {
		String betType = null;
		Scanner inputScanner = new Scanner(System.in);
		System.out.print("How much cash do you have? ");
		if(inputScanner.hasNextInt())
		{
			int amountOfCash = inputScanner.nextInt();
			if(amountOfCash<=0)
			{
				System.out.println("Error: enter a valid amount");
			}

			Wallet playerCash = new Wallet();
			playerCash.put(amountOfCash);

			boolean quit = false;
			while(!quit) {

				if (playerCash.check() <= 0)
					quit = true;

				System.out.print("What do you want to bet -'Triple', 'Field', 'High' or 'Low'? (or enter 'quit' to finish) ");	
				if(inputScanner.hasNextInt() || inputScanner.hasNextDouble())
				{
					System.out.println("Error invalid selection please try again");
				}

				betType = inputScanner.next();

				if (betType.equals("quit")) 
				{
					quit = true;
				}
				else
				{
					resolveBet(betType, playerCash); 
				}

			}

			inputScanner.close();
			System.out.println("Game over you started with " + amountOfCash + " now have " +(int) playerCash.check());		

		}
	}

}



