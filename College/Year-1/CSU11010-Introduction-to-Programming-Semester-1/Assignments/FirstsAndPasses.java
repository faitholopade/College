/*  SELF ASSESSMENT of whether my code is easy to understand.
   1. Did I use easy-to-understand meaningful variable and constant names?
       Mark out of 10: 10
       Comment: Yes I used easy-to-understand meaningful variable and constant names
   2. Did I format the variable and constant names properly (in lowerCamelCase and UPPERCASE)?
       Mark out of 5: 5   
       Comment: Yes I formatted the variable and constant names properly
   3. Did I indent the code appropriately?
       Mark out of 10: 10   
       Comment: Yes I indented the code appropriately
   4. Did I implement a for loop to read the input as required?
       Mark out of 10: 10  
       Comment: Yes I implemented  a for loop to read the input as required
      Total Mark out of  35 (Add all the previous marks): 35
*/
import java.util.Scanner;

public class FirstsAndPasses {

	public static void main(String[] args) {
		
        System.out.print("Enter the number of students in the class> ");
		Scanner inputScanner = new Scanner( System.in );
		int numberOfStudents = inputScanner.nextInt();
		
		if (numberOfStudents < 1)
		{ 
			System.out.print ("Error:  The number of students must be greater than 0");
		}
		
		else {
				double percentageObtained = 0.0;
				int studentNumber;
				 
				int numberPassedClass = 0;
				int numberOfFirsts = 0;
		
		    for (studentNumber = 1; (studentNumber <= numberOfStudents); studentNumber = studentNumber + 1)
			{ 
				System.out.print("\nEnter the percentage obtained by student "+ studentNumber + "> ");
		        percentageObtained = inputScanner.nextDouble();
		        
		    final double MINIMUM_FIRST_CLASS_PERCENTAGE = 70.0;
		    final double MINIMUM_PASS_PERCENTAGE = 40.0;
		    
		        if (Math.round(percentageObtained) >=MINIMUM_FIRST_CLASS_PERCENTAGE)
			    { 
		        	numberOfFirsts ++;
			    }
		        if (Math.round(percentageObtained)>= MINIMUM_PASS_PERCENTAGE)
			    {
		        	numberPassedClass ++;
			    }
		        
			
		    final double MINIMUM_MARK = 0.0;
		    final double MAXIMUM_MARK = 100.0;
		    
		        for (percentageObtained = percentageObtained; (percentageObtained <= MINIMUM_MARK || 
		        	 percentageObtained >  MAXIMUM_MARK);)
		        {
		        	System.out.println("\nInvalid percentage.  Please enter the mark again for this student."); 
		
		        	System.out.print("\nEnter the percentage obtained by student "+ studentNumber + "> ");
		        	percentageObtained = inputScanner.nextDouble();
		        }
	        }
		    
	        final double CONVERSION_TO_PERCENT = 100.0;
	        
	        double percentagePassed = ((double) numberPassedClass / (double) numberOfStudents) * CONVERSION_TO_PERCENT;
	        
	    	System.out.printf(
	    	("\nIn this class of " + numberOfStudents + " students, "+ numberOfFirsts + 
	    	" got a first class honour and %.1f%% passed the class"), percentagePassed);
	       
			 
			}
		inputScanner.close();
	       
			 }
				
		  }
		





		
	


	