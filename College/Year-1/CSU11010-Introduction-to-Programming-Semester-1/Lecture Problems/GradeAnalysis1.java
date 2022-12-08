import java.util.Scanner;

/*
 * Write a program which analyses the percentages obtained by a class of 5 students.
 * The user should enter a list of percentages and the program should determine
 * the average mark and the number of students who got above the average mark.  E.g:
 *
 *   Enter the 5 percentages obtained: 65.2 45.6 55.8 60.9 99.5
 *   The average percentage is 65.4% and there was only 1 above average student.
 *
 * As part of your solution you must write and use the following routines:
 * - double determineAverageMark( double[ ] marks ) which takes an array containing
 *   the marks and returns the average grade.
 * - int countAboveAverageStudents( double[ ] marks, double average ) which takes an
 *   array containing the marks together with the average mark achieved, and returns
 *   the number of students who achieved above the average mark.
 */
public class GradeAnalysis1 {

	public static final int NUMBER_OF_MARKS=5;
	public static final double MIN_MARK=0.0;
	public static final double MAX_MARK=100.0;
	
	public static double determineAverageMark( double[ ] marks )
	{
		if (marks == null)
			return 0.0;
		double totalMarks = 0;
		for (int index=0; index<marks.length; index++)
			totalMarks += marks[index];
		return totalMarks/marks.length;
	}
	
	public static int countAboveAverageStudents( double[ ] marks, double average )
	{
		if (marks == null)
			return 0;
		int aboveAverageCount = 0;
		for (int index=0; index<marks.length; index++)
			if (marks[index] > average)
				aboveAverageCount++;
		return aboveAverageCount;
	}

	public static void main(String[] args) {
		
		System.out.print("Enter the " + NUMBER_OF_MARKS + " percentages obtained: ");
		Scanner marksScanner = new Scanner( System.in );
		int marksCount = 0;
		double percentages[] = new double[NUMBER_OF_MARKS];
		while ((marksCount < NUMBER_OF_MARKS) && (marksScanner.hasNextDouble()))
		{
			double currentMark = marksScanner.nextDouble();
			if ((currentMark >= MIN_MARK) && (currentMark <= MAX_MARK) &&
					(marksCount <= NUMBER_OF_MARKS))
			{
				percentages[marksCount] = currentMark;
				marksCount++;
			}
		}
		if (marksCount != NUMBER_OF_MARKS)
		{
			System.out.println("Invalid marks entered.  " + NUMBER_OF_MARKS +
					"marks should be entered between " + MIN_MARK + " and " + MAX_MARK + ".");
		}
		else
		{
			double averageMark = determineAverageMark( percentages );
			int numberAboveAverage = countAboveAverageStudents( percentages, averageMark );
			System.out.println("The average percentage is " + averageMark + "% and there " +
			  ((numberAboveAverage > 1) ? "were " + numberAboveAverage + " above average students." :
			      ((numberAboveAverage == 1) ? "was only 1 above average student." :
			    	  "were no above average students.")));
		}
	}
}