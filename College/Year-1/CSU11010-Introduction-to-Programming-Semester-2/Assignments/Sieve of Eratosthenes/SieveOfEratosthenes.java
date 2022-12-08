/* SELF ASSESSMENT 
   1.    createSequence:
Did I use the correct method definition?
Mark out of 5: 5
Comment: Yes I used the correct method definition
Did I create an array of size n (passed as the parameter) and initialise it?
Mark out of 5: 5
Comment: Yes I created an array of size n and initialized it
Did I return the correct item?
Mark out of 5: 5
Comment: Yes I created an array of size n and initialized it 
   2.    crossOutMultiples
Did I use the correct method definition?
Mark out of 5: 5
Comment: Yes I used the correct method definition
Did I ensure the parameters are not null and one of them is a valid index into the array
Mark out of 2: 2
Comment: Yes I ensured the parameters are not null and one of them is a valid index into the array
Did I loop through the array using the correct multiple?
Mark out of 5: 5
Comment: Yes I looped through the array using the correct multiple
Did I cross out correct items in the array that were not already crossed out?
Mark out of 3: 3
Comment: Yes I crossed out the correct items in the array that were not already crossed out
   3.    sieve   
Did I have the correct function definition?
Mark out of 5: 5
Comment: Yes I had the correct function definition
Did I make calls to other methods?
Mark out of 5: 5
Comment: Yes I made calls to other functions
Did I return an array with all non-prime numbers are crossed out?
Mark out of 2: 2
Comment: Yes I returned an array with all non-prime numbers crossed out
   4.    sequenceTostring  
Did I have the correct function definition?
Mark out of 5: 5
Comment: Yes I had the correct function definition
Did I ensure the parameter to be used is not null?
Mark out of 3: 3
Comment: I ensured the parameter to be used is not null
Did I Loop through the array updating the String variable with the non-crossed out numbers and the crossed numbers in brackets? 
Mark out of 10: 10
Comment: Yes I looped through the array updating the String variable with non crossed out numbers and the crossed numbers in brackets
   5.    nonCrossedOutSubseqToString  
Did I have the correct function definition
Mark out of 5: 5
Comment:  Yes   
Did I ensure the parameter to be used is not null?  
Mark out of 3: 3
Comment: Yes
Did I loop through the array updating the String variable with just the non-crossed out numbers? 
Mark out of 5: 5
Comment: Yes
   6.    main  
Did I ask  the user for input n and handles input errors?  
Mark out of 5: 5
Comments: Yes
Did I make calls to other methods (at least one)?
Mark out of 5: 5
Comment: Yes
Did I print the output as shown in the question?  
Mark out of 5: 5
Comment:  Yes
   7.    Overall
Is my code indented correctly?
Mark out of 4: 4
Comments: Yes
Do my variable names make sense?
Mark out of 4: 4
Comments: Yes
Do my variable names, method names and class name follow the Java coding standard
Mark out of 4: 4
Comments: Yes
      Total Mark out of 100 (Add all the previous marks): 100
 */
import java.util.Scanner;

public class SieveOfEratosthenes {
	public static int[] createSequence(int N) 
	{
		int sequenceIndex = 0;
		int[] sequenceOfNumbers = new int[N -1];
		for (int index = 2; index <= N; index++) 
		{
			sequenceOfNumbers[sequenceIndex] = index;
			sequenceIndex++;
		}
		return sequenceOfNumbers;

	}

	public static int[] crossOutHigherMultiples(int[] sequenceOfNumbers, int n) 
	{
		if(sequenceOfNumbers == null)
		{
			System.out.print("Error array can not be null");
			return null;
		}
		else
		{
			int[] noMultiplesSequence = sequenceOfNumbers.clone();
			for (int j = (n*2 - 2); j < (sequenceOfNumbers.length); j = (j + n)) 
			{
				noMultiplesSequence[j] = 0;
			}
			return noMultiplesSequence;
		}
	}

	public static int[] sieve(int N) 
	{
		int[] sequenceOfNumbers = createSequence(N);

		int[] sieve = sequenceOfNumbers.clone();
		for (int index = 2; index <= N - 2; index++) 
		{
			if(sieve[index - 2]!= 0)
			{
				sieve = crossOutHigherMultiples(sieve, index);
				if (index < Math.round(Math.sqrt(N)))
				{
					System.out.println(sequenceToString(sieve));
				}
			}
		}
		return sieve;


	}

	public static String sequenceToString(int[] sieveSequence) {
		if(sieveSequence == null)
		{
			System.out.print("Error array can not be null");
			return null;
		}
		else
		{
			StringBuilder sequenceOfNumbersArray = new StringBuilder();
			sequenceOfNumbersArray.append(2);
			for (int index = 1; index< sieveSequence.length; index++) 
			{
				if(sieveSequence[index] == 0) 
				{
					sequenceOfNumbersArray.append(", [");
					sequenceOfNumbersArray.append(index + 2);
					sequenceOfNumbersArray.append("]");

				} 
				else 
				{
					sequenceOfNumbersArray.append(", ");
					sequenceOfNumbersArray.append(sieveSequence[index]);
				}
			}
			return sequenceOfNumbersArray.toString();
		}
	}

	public static String nonCrossedOutSubseqToString(int[] sieveSequence) {
		if(sieveSequence == null)
		{
			System.out.print("Error array can not be null");
			return null;
		}
		else
		{
			StringBuilder sequenceOfNumbersArray = new StringBuilder();
			sequenceOfNumbersArray.append(2);
			for (int index = 1; index < sieveSequence.length; index++) 
			{
				if (sieveSequence[index] != 0)
				{
					sequenceOfNumbersArray.append(", ").append(sieveSequence[index]);
				}

			}
			return sequenceOfNumbersArray.toString();
		}
	}



	public static void main(String[] args) 
	{
		Scanner inputScanner = new Scanner(System.in);
		System.out.print("Enter int >= 2 :");
		int N = inputScanner.nextInt();
		if (N < 2) 
		{
			System.out.println("Error: Enter a number >= 2");
		} 
		else 
		{
			System.out.println(sequenceToString(createSequence(N)));
			System.out.println(sequenceToString(sieve(N)));
			System.out.println(nonCrossedOutSubseqToString(sieve(N)));
		}

	}


}


