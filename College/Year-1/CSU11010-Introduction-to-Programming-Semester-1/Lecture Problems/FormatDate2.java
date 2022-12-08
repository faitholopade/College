/*
 * Convert the month entered as a number into a
word using arrays. 
 */
import java.util.Scanner;
public class FormatDate2 {
public static final String[] MONTHS = { "January", "February",
"March", "April", "May", "June", "July", "August",
"September", "October", "November", "December" };
public static void main(String[] args) {
System.out.print("Enter the month as a number: ");
Scanner input = new Scanner( System.in );
int month = input.nextInt();
if ((month >= 1) && (month <= 12))
System.out.println("The month is " + MONTHS[month-1] );
}
}
