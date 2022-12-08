import java.util.Scanner;

public class PowerOf {
public static void main(String[] args) {
Scanner input = new Scanner( System.in );
System.out.print("Enter base: ");
if (!input.hasNextInt())
{
System.out.println("Error: You must enter a whole number");
}
else
{
int base = input.nextInt();
System.out.print("Enter exponent: ");
if (!input.hasNextInt())
{
System.out.println("Error: You must enter a whole number");
}
else
{
int exponent = input.nextInt();
if (exponent < 0)
{
System.out.println("Error: Exponent must be positive");
}
else
{
int result = 1;
for (int count=0; (count < exponent); count++)
{
result *= base;
}
System.out.println("" + base +
" to the power of " + exponent + " is " + result);
}
}
}
}
}