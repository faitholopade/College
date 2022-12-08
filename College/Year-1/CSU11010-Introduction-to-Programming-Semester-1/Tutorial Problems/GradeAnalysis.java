//Write a program which takes in any number of
//grades until the user enters “quit”, and computes
//the percentage of first class honours after each
//grade is entered.
//Ensure that all possible error conditions are
//handled.
import java.util.Scanner;
public class GradeAnalysis {
public static final double MINIMUM_FIRST_CLASS_PERCENTAGE = 70.0;
public static final double MINIMUM_MARK = 0.0;
public static final double MAXIMUM_MARK = 100.0;
public static void main(String[] args) {
int numberOfGradesEntered = 0;
int numberOfFirsts = 0;
boolean quit = false;
Scanner input = new Scanner( System.in );
do
{
System.out.print("Enter a mark (out of 100.0) or enter 'quit': ");
if (input.hasNextDouble())
{ 
double mark = input.nextDouble();
if ((mark >= MINIMUM_MARK) && (mark <= MAXIMUM_MARK))
{
numberOfGradesEntered++;
numberOfFirsts += (Math.round(mark) >= MINIMUM_FIRST_CLASS_PERCENTAGE)
? 1 : 0;
double percentageOfFirsts = 100.0*((double)numberOfFirsts)/
((double)numberOfGradesEntered);
System.out.printf(
"So far %.0f%% students have obtained first class honours.\n",
percentageOfFirsts);
}
else System.out.printf(
" Error: The mark must be between %.1f and %.1f\n",
 MINIMUM_MARK,MAXIMUM_MARK);
}
else if (input.hasNext("quit"))
quit = true;
else
{
System.out.printf(
" Error: You must enter a number between %.1f and %.1f\n",
MINIMUM_MARK,MAXIMUM_MARK);
input.next();
}
} while (!quit);
input.close();
}
} 
