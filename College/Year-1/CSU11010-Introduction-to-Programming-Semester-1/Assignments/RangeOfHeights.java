/*  SELF ASSESSMENT of whether my code is easy to understand.
   1. Did I use easy-to-understand meaningful variable names?
       Mark out of 5: 5   
       Comment:  Yes I used easy-to-understand meaningful variable names
   2. Did I format the variable names properly (in lowerCamelCase)?
       Mark out of 5: 5   
       Comment: Yes I formatted the variable names properly
   3. Did I indent the code appropriately?
       Mark out of 5: 5  
       Comment: Yes I indented the code appropriately
   4. Did I implement a while loop or do-while loop as required?
       Mark out of 10: 10   
       Comment: Yes I implemented a while loop as required
      Total Mark out of  25 (Add all the previous marks):  25
*/

import java.util.Scanner;

public class RangeOfHeights {

	public static void main(String[] args) {
		
		   Scanner inputScanner = new Scanner( System.in );
		
		final double VALUE_TO_TERMINATE = 0.0;
		double minimumHeight = Double.MAX_VALUE;
		double maximumHeight = Double.MIN_VALUE;
		boolean finish = false;
		
		while (!finish)
		{
			double userHeight;
				
			System.out.print("Enter a height in metres (0.0 to finish)> " );
			userHeight = inputScanner.nextDouble();
			
			if (userHeight == VALUE_TO_TERMINATE)
			{
				finish = true;
			}
			else if (userHeight < VALUE_TO_TERMINATE)
			{
				System.out.println("Negative heights are invalid.");	
			}
			else 
			{
				
				if (userHeight < minimumHeight)
					minimumHeight = userHeight;
				if (userHeight > maximumHeight)
					maximumHeight = userHeight;
				
				System.out.println("Heights entered so far are in the range " + minimumHeight 
						+ " to " + maximumHeight);
				
			}
		
	
		}	
		
		inputScanner.close();
		
		}

	}




