import java.util.Scanner;
public class DigitFrequencies {
	public static final int NUMBER_OF_DIGITS = 10;
	public static void main(String[] args) {
		boolean finished = false;
		// Initialise frequencies
		int[] digitFrequencies = new int[NUMBER_OF_DIGITS];
		for (int index=0; index<NUMBER_OF_DIGITS; index++)
			digitFrequencies[index]=0;
		Scanner input = new Scanner( System.in );
		while(!finished)
		{

			System.out.print("Enter a number> ");
			if (input.hasNextInt())
			{
				int number = input.nextInt();
				countDigitFrequencies( number, digitFrequencies );
				printDigitFrequencies( digitFrequencies );
			}
			else {
				System.out.print("Error - Enter any real number or quit.\n");
				input.next();
			}
		} 
		input.close();
	}


	public static void printDigitFrequencies( int[] frequencies )
	{
		System.out.print("Digit Frequencies: ");
		if (frequencies != null)
		{
			boolean commaNeeded = false;
			for (int index=0; index<NUMBER_OF_DIGITS; index++)
				if (frequencies[index] > 0)
				{
					System.out.print( (commaNeeded ? ", ":"") + index + "(" + frequencies[index] + ")" );
					commaNeeded = true;
				}
		}
		System.out.println();
	}

	public static void countDigitFrequencies( int number, int[] frequencies )
	{
		int remainingNumber = Math.abs(number);
		while(remainingNumber > 0)
		{
			int digit = remainingNumber%10;
			remainingNumber /= 10;
			frequencies[digit]++;
		} 
	}

}