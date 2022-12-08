//Write a program which computes the Body Mass Index (BMI) of
//a person and determines their weight classification:
//• Underweight (BMI less than 18.5),
//• Normal (BMI between 18.5 and 24.99999),
//• Overweight (BMI between 25.0 and 29.99999),
//• or Obese (BMI of 30 or more).
//The BMI is computed by dividing the weight of a person in
//kilograms by the square of the height of the person in metres.
import java.util.Scanner;	
public class	BMI2	{	
public static final double MIN_NORMAL_BMI	=	18.5;	
public static final double MIN_OVERWEIGHT_BMI	=	25.0;	
public static final double MIN_OBESE_BMI	=	30.0;	
public static void	main(String[]	args)	{	
Scanner	input	=	new	Scanner(	System.in	);	
System.out.print("What	is	your	weight	in	kg?	");	
double	weight	=	input.nextDouble();	
System.out.print("What	is	your	height	in	metres?	");	
double	height	=	input.nextDouble();	
double bmi	=	weight	/	(height*height);
String	weightClassification	=	"";	
if	(bmi	<	MIN_NORMAL_BMI)	
{	
weightClassification	=	"Underweight";	
}	
else if	(bmi	<	MIN_OVERWEIGHT_BMI)	
{	
weightClassification	=	"Normal";	
}	
else if	(bmi	<	MIN_OBESE_BMI)	
{	
weightClassification	=	"Overweight";	
}	
else		
{	
weightClassification	=	"Obese";	
}
System.out.println("Your	BMI	is	"	+	bmi	+	
"	which	is	classified	as	"	+	weightClassification	);
}
}