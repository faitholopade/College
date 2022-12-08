import java.util.Scanner;

public class MarksRemapper {

	public static double determineAverage( double[] arrayOfNumbers )
	{	
		double initialAverage = 0.0;
		double sum = 0.0;

		if (arrayOfNumbers!=null)
		{
			int length = arrayOfNumbers.length;
			for (int i = 0; i < arrayOfNumbers.length; i++) 
			{
				sum = sum + arrayOfNumbers[i];
			}
			if (arrayOfNumbers.length!=0)
				initialAverage = sum / length;
			return initialAverage;	
		}

		else
		{
			return sum;
		}

	}

	public static double determineStandardDeviation( double[] arrayOfNumbers, double average ) 
	{
		if (arrayOfNumbers != null)
		{
			double initialstandardDeviation = 0.0;
			if (arrayOfNumbers.length!=0) 
			{
				double sumOfSquaredDifferences = 0.0;
				for (int index=0; index < arrayOfNumbers.length; index++)
				{
					sumOfSquaredDifferences += Math.pow(arrayOfNumbers[index]-average,2.0);
				}
				initialstandardDeviation = Math.sqrt( sumOfSquaredDifferences/
						((double) arrayOfNumbers.length));
			}
			return initialstandardDeviation;
		}

		else
		{
			return 0.0;
		}
	}

	public static boolean modifyAllMarks( double[] arrayOfMarks, double desiredAverage, double desiredStandardDeviation ) 
	{
		if(arrayOfMarks!=null)
		{
			double[] temporaryArray = new double[arrayOfMarks.length];
			double initialAverage = determineAverage(arrayOfMarks);
			double initialStandardDeviation = determineStandardDeviation(arrayOfMarks, initialAverage);

			for(int i = 0; i < arrayOfMarks.length; i++ )
			{
				double 	initialMark = arrayOfMarks[i];
				double desiredMark = 0.0;

				if(initialStandardDeviation!=0)
				{
					desiredMark = desiredAverage + (initialMark - initialAverage) * desiredStandardDeviation / initialStandardDeviation;
				}

				else
				{
					desiredMark = desiredAverage + (initialMark - initialAverage) * desiredStandardDeviation;
				}

				if(desiredMark < 0)
				{
					desiredMark = 0;
				}

				else if(desiredMark > 100)
				{
					desiredMark = 100;
				}

				temporaryArray[i] = desiredMark;
				System.out.print(initialMark + "->");
				System.out.printf("%.1f", desiredMark);

				if(i!=arrayOfMarks.length - 1)
				{
					System.out.print(", ");
				}

			}

			if(arrayOfMarks!= null)
			{
				System.arraycopy(temporaryArray, 0, arrayOfMarks, 0, temporaryArray.length);
			}

			double modifiedAverage = determineAverage(arrayOfMarks);
			double modfiedStandardDeviation = determineStandardDeviation(arrayOfMarks, modifiedAverage);

			return true;
		}

		else
		{
			return false;
		}

	}

	public static void main(String[] args) {
		System.out.print("Enter all percentages (separated by spaces): ");
		Scanner inputScanner = new Scanner (System.in);
		Scanner inputLine = new Scanner(inputScanner.nextLine());
		double [] arrayOfMarks = null;

		do
		{
			if(inputLine.hasNextDouble())
			{
				double userMark = inputLine.nextDouble();
				double [] temporaryArray = new double [(arrayOfMarks == null)? 1 : arrayOfMarks.length + 1];
				if(arrayOfMarks!= null)
				{
					System.arraycopy(arrayOfMarks, 0, temporaryArray, 0, arrayOfMarks.length);
				}

				temporaryArray[temporaryArray.length - 1] = userMark;
				arrayOfMarks = temporaryArray;
			}

			else
			{
				String invalidInput = inputLine.next();
				System.out.println("Error: " +invalidInput+ "is not a valid number.");
			}	

		}while(inputLine.hasNextDouble()|| inputLine.hasNext());

		double initialAverage = determineAverage(arrayOfMarks);
		double initialStandardDeviation = determineStandardDeviation(arrayOfMarks, initialAverage);
		if(arrayOfMarks!= null)
		{
			System.out.printf("The original average was %.1f and the standard deviation was %.1f\n",initialAverage,
					initialStandardDeviation);
			boolean modifyAllMarks = modifyAllMarks(arrayOfMarks, 60, 20);
			double modifiedAverage = determineAverage(arrayOfMarks);
			double modifiedStandardDeviation = determineStandardDeviation(arrayOfMarks, modifiedAverage);
			System.out.printf("The new average is %.1f and the new standard deviation is %.1f",modifiedAverage,
					modifiedStandardDeviation);

		}

		inputScanner.close();


	}

}
 