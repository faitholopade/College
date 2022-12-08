import java.util.Scanner;
public class ETest1 {
	public static int REPOSITION_DIGIT = 1;
	public static int TERMINATE_PROGRAM = -1;
	public static int ISOLATE_DIGITS = 10;
	public static int INCREMENTER = 1;
	public static int COUNT_VALUE_AFTER_FIRST_DIGIT = 2;
	public static int RESET_NUMBER_OF_EQUAL_DIGITS = 1;
	public static int FIRST_NUMBER_NUDE_AND_ZYGO = 11;
	public static boolean isNude(int number)
	{
		int tempNumber = number;
		int currentDigit = 0;
		while(tempNumber != 0)
		{
			currentDigit = tempNumber % ISOLATE_DIGITS ;
			if(currentDigit == 0)
			{
				return false;
			}
			number = number / ISOLATE_DIGITS ;
			if(number % currentDigit != 0)
			{
				return false;
			}
		}
		return true;
	}
	public static int getNumberOfDigits(int number)
	{
		int countNumberOfDigits = 0;
		while(number != 0)
		{
			number = number / ISOLATE_DIGITS ;
			countNumberOfDigits = countNumberOfDigits + INCREMENTER ;
		}
		return countNumberOfDigits ;
	}
	public static int getDigit(int number, int digitNumber)
	{
		int totalDigitsInNumber = getNumberOfDigits(number);
		int relevantDigit = 0;
		if(totalDigitsInNumber < digitNumber)
		{
			return TERMINATE_PROGRAM;
		}
		else
		{
			int remainingDigits = totalDigitsInNumber - digitNumber + REPOSITION_DIGIT;
			for(int count = 1; count <= remainingDigits; count = count + INCREMENTER)
			{
				relevantDigit = number % ISOLATE_DIGITS ;
				number = number / ISOLATE_DIGITS ;
			}
			return relevantDigit;
		}
	}
	public static boolean isZygodrome(int number)
	{
		int lastDigitOfNumber = number % ISOLATE_DIGITS;
		int count = COUNT_VALUE_AFTER_FIRST_DIGIT;
		int numberOfEqualDigits = 1;
		while(getDigit(number,count)
				!= TERMINATE_PROGRAM)
		{
			int currentDigit = getDigit(number,count);
			if(lastDigitOfNumber == currentDigit)
			{
				numberOfEqualDigits = numberOfEqualDigits + INCREMENTER;
			}
			else if (numberOfEqualDigits < COUNT_VALUE_AFTER_FIRST_DIGIT )
				return false;
			numberOfEqualDigits = RESET_NUMBER_OF_EQUAL_DIGITS;
			lastDigitOfNumber = getDigit(number,count);
			count = count + INCREMENTER;
		}
		if (numberOfEqualDigits==1)
		{
			return false;
		}
		else
		{
			return true;
		}
	}
	public static void main(String[] args) {
		Scanner inputScanner = new Scanner(System.in);
		boolean terminateLoop = false;
		int userNumber = 0;
		while(
				!terminateLoop)
		{
			System.out.print("Enter the maximum positive whole number to be considered (or -1 to quit)> ");
			if (inputScanner.hasNextInt()
					)
			{
				userNumber = inputScanner.nextInt();
				if(userNumber == -1)
				{
					terminateLoop = true;
				}
				else if (userNumber < 1)
				{
					System.out.println( "Error: "+userNumber+" is not a positive number.");
				}
				else
				{
					if (userNumber < FIRST_NUMBER_NUDE_AND_ZYGO)
					{
						System.out.println( "There are no numbers which are both zygodromes and nude up to " +
								userNumber);
					}
					else
					{
						System.out.println( "The numbers which are both zygodromes and nude up to " +userNumber+" are "
								);
						boolean multipleZygos = false;
						int currentNumber = 0;
						for(currentNumber = 1; currentNumber <= userNumber; currentNumber = currentNumber + INCREMENTER
								);
						{
							if(isNude(currentNumber) && isZygodrome(currentNumber)
									)
							{
								if(multipleZygos)
								{
									System.out.print(", ");
								}
								multipleZygos = true;
								System.out.print(currentNumber);
							}
						}
					}
				}
			}
			else
			{
				String invalidInput = inputScanner.next();
				System.out.println( "Error: "+invalidInput+" is not a positive whole number.");
			}
		}
	}
}
