/*  SELF ASSESSMENT of whether my code is easy to understand.
   1. Did I use appropriate CONSTANTS instead of numbers within the code?
       Mark out of 5:    5
       Comment: Yes, I used appropriate constants instead of numbers within the code
   2. Did I use easy-to-understand, meaningful CONSTANT names?
       Mark out of 5:   5
       Comment: Yes, the names I used are easy to understand and relate directly to key information.
   3. Did I format the CONSTANT names properly (in UPPERCASE)?
       Mark out of 5:  5 
       Comment: Yes
   4. Did I use easy-to-understand meaningful variable names?
       Mark out of 10:   
       Comment: Yes, the names I used are easy to understand and relate directly to key information.
   5. Did I format the variable names properly (in lowerCamelCase)?
       Mark out of 5:   
       Comment: Yes I formated the variable names properly (in lowerCamelCase)
   6. Did I indent the code appropriately?
       Mark out of 10: 10  
       Comment: Yes I indented the code appropriately 
     Total Mark out of  40 (Add all the previous marks):  40
*/

import java.text.DecimalFormat;
import java.util.Scanner;

public class StoppingDistance1 {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		
		 Scanner inputScanner = new Scanner (System.in);
		 

         System.out.println("Enter vehicle speed in km/hr>");
  double speed = inputScanner.nextDouble();
 
         System.out.println("Enter reaction time in seconds>");
  double reactionTime = inputScanner.nextDouble();
  
  final double CONVERSION_KMPERHR_TO_MTRSPERSEC = 3.6;
 
  double stoppingDistance = speed / CONVERSION_KMPERHR_TO_MTRSPERSEC * reactionTime + 
		 ((speed / CONVERSION_KMPERHR_TO_MTRSPERSEC) 
		 * (speed / CONVERSION_KMPERHR_TO_MTRSPERSEC)) / 20.0;
 
        
       DecimalFormat df = new DecimalFormat (".#");
       
       
        		 
   System.out.printf (" At a speed of " + df.format(speed) + "km/hr" +
      " and with a reaction time of " +  df.format(reactionTime) +
       "s," + " the stopping distance would be " + df.format(stoppingDistance) +  "m");
         

{
}  
 
 
	
 

 
 

	}

}
