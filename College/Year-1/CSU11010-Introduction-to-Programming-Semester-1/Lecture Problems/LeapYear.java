// Write	a	program	to	determine	if	the	year	entered	is	
//a	leap	year	or	not.		A	year	is	a	leap	year	if	it	is	
//divisible	by	4		(e.g.	the	year	2012	is	a	leap	year)	
//unless	it	is	divisible	by	100	(e.g.	the	year	2100	is	not	
//a	leap	year)	unless	it	is	divisible	by	400	(e.g.	the	year	
//2000	is	a	leap	year)

import java.util.Scanner;
public class	LeapYear	{
		public static void	main(String[]	args)	{
				System.out.print("Enter	the	year?	");	
				Scanner	input	=	new	Scanner(	System.in	);	
				int	year	=	input.nextInt();
				boolean	isLeapYear	=	false;
				if	(year	%	400	==	0)	
						isLeapYear	=	true;	
				else if	(year	%	100	==	0)	
						isLeapYear	=	false;	
				else if	(year	%	4	==	0)	
						isLeapYear	=	true;	
				else		
						isLeapYear	=	false;
				if	(isLeapYear)
System.out.print("Year	"	+	year	+"	is	a	leap	year.");	
			else	System.out.print("Year	"	+	year	+	"	is	not	a	leap	year.");		
		}
}	