import java.util.Scanner;

public class ETestSample {
	
	public static int computeNextProperDivisor(int number, int lastProperDivisor) 
	{
		int divisor = lastProperDivisor + 1;
		while (divisor != number) 
		{
			if (number % divisor == 0)
			{
				return divisor;
			}
			
			divisor++;
		}
		return -1;
	}

	public static int sumOfProperDivisors(int number) 
	{
		int sum = 0;
		int divisor = 0;
		boolean noMoreDivisors = false;
		
		while (!noMoreDivisors)
		{
			if (computeNextProperDivisor(number,divisor)==-1)
			{
				noMoreDivisors=true;
			}
		
			else 
			{
				divisor = computeNextProperDivisor(number,divisor);
				sum += divisor;
			}
		}
		return sum;
	}

	public static String isDeficientPerfectOrAbundant(int number) 
	{
		int sumOfDivisors = sumOfProperDivisors(number);

		if (sumOfDivisors < number)
		{
			return "Deficient";
		}
		else if (sumOfDivisors==number)
		{
			return "Perfect";
		}	
		else
		{
			return "Abundant";
		}
	}

	public static boolean isAdmirable(int number) 
	{
		int sumOfDivisors = sumOfProperDivisors(number);
		boolean noMoreDivisors = false;
		int currentProperDivisor = 0;
		while (!noMoreDivisors) 
		{
			currentProperDivisor = computeNextProperDivisor(number,currentProperDivisor);

			if (currentProperDivisor!=-1) 
			{
				int result = sumOfDivisors - currentProperDivisor * 2;

				if (result==number)
				{	
					return true;
				}
			}
			else
			{
				noMoreDivisors = true;
			}
		}
		return false;

	}
	public static void main(String[] args) {
		Scanner input = new Scanner(System.in);
		boolean stopLoop = false;
		while (!stopLoop) 
		{
			System.out.print("Enter the positive whole number to be considered (or 'quit')> ");
			int number = 0;
			
			if (input.hasNextInt()) 
			{
				number = input.nextInt();
				
				if (number < 1)
				{
					System.out.println("Error: "+number+" is not a positive whole number.");
				}
				else 
				{
					System.out.print(number+" is "+isDeficientPerfectOrAbundant(number)+" and is ");
					
					if (!isAdmirable(number))
					{
						System.out.print("not ");
					}
					System.out.println("Admirable");
					
				}
			}
			else 
			{
				String word = input.next();
				if (word.equalsIgnoreCase("quit"))
				{
					stopLoop = true;
				}
				else
				{
					System.out.println(" Error: "+word+" is not a positive whole number.");
				}
			}
		}
		input.close();
	}

	}


