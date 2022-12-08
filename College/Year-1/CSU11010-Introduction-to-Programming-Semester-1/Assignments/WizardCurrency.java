import java.util.Scanner;

public class WizardCurrency {

	public static int convertGBPToKnuts( int pounds, int pence )
	{
		int totalNoOfPence = 100 * pounds + pence;
		double initialValue = Math.round(((812.0 * totalNoOfPence) / 499.0));
		int totalNoOfKnuts = (int)(initialValue);

		return totalNoOfKnuts;
	}

	public static String convertKnutsToWizardingCurrency( int totalKnuts )
	{
		int sickles = totalKnuts / 29;
		totalKnuts = totalKnuts - 29 * sickles;
		int galleons = sickles / 17;
		sickles = sickles - 17 * galleons;
		String WizardingCurrency = "HP" + galleons + ":" + sickles + ":" + totalKnuts;

		return WizardingCurrency;
	}

	public static String convertGBPToWizardingCurrency( int pounds, int pence )
	{
		double knuts = convertGBPToKnuts(pounds,pence);
		double sickles = knuts/29.0;
		knuts = knuts-((int)(sickles) * 29.0);
		double galleons = sickles / 17.0;
		sickles = sickles-((int)(galleons) * 17.0);
		int integerKnuts = (int)(knuts);
		int integerSickles = (int)(sickles);
		int integerGalleons = (int)(galleons);
		String WizardingCurrency = "HP"+ integerGalleons + ":" + integerSickles + ":" + integerKnuts;

		return WizardingCurrency;
	}

	public static int convertWizardingCurrencyToKnuts( int numGalleons, int numSickles, int numKnuts )
	{
		int knuts = numGalleons * 493 + numSickles * 29 + numKnuts;

		return knuts;
	}

	public static int convertWizardingCurrencyToPence( int numGalleons, int numSickles, int numKnuts )
	{
		int knuts = convertWizardingCurrencyToKnuts(numGalleons,numSickles,numKnuts);
		int noOfPence = (int)(Math.round((double)(knuts * 499.0 / 812.0)));

		return noOfPence;
	}

	public static double getNumberFromUser( String prompt, String errorMessage, Scanner input, boolean integerRequired )
	{
		double number = 0;
		boolean quitProgram = false;
		do
		{
			System.out.print(prompt);
			if (integerRequired && input.hasNextInt()) 
			{
				number = (double)(input.nextInt());
				quitProgram = true;
			}
			else if (input.hasNextDouble() && !integerRequired) 
			{
				number = input.nextDouble();
				quitProgram = true;
			}
			else 
			{
				System.out.println(errorMessage);
				input.nextLine();
			}


		} while (!quitProgram);


		return number;
	}




	public static void main(String[] args) {
		Scanner inputScanner = new Scanner (System.in);
		int userFunctionSelection  = 0;
		do {
			String prompt="1. Convert Wizarding Currency To GBP\n" + 
					"2. Convert Knuts To Wizarding Currency and GBP\n" + 
					"3. Convert GBP to Wizarding Currency\n" + 
					"4. Quit\n"+"Select function> ";
			String errorMessage="Error: Invalid selection.";
			boolean integerRequired=true;
			userFunctionSelection = (int)(getNumberFromUser( prompt, errorMessage, inputScanner, integerRequired ));
			if (userFunctionSelection >= 1 &&userFunctionSelection <= 4) 
			{
				if (userFunctionSelection == 1) 
				{ 
					integerRequired = true;
					prompt = "Enter the number of Galleons > ";
					errorMessage = "Error: The number of Galleons should be an integer (e.g. 3)";
					int galleons = (int)(getNumberFromUser( prompt, errorMessage, inputScanner, integerRequired ));
					prompt = "Enter the number of Sickles > ";
					errorMessage = "Error: The number of Sickles should be an integer (e.g. 5)";
					int sickles = (int)(getNumberFromUser( prompt, errorMessage, inputScanner, integerRequired ));
					prompt = "Enter the number of Knuts > ";
					errorMessage = "Error: The number of Knuts should be an integer (e.g. 12)";
					int knuts = (int)(getNumberFromUser( prompt, errorMessage, inputScanner, integerRequired ));
					double conversionValue = (double)(convertWizardingCurrencyToPence(galleons,sickles,knuts));
					conversionValue = conversionValue /100.0;
					System.out.println("In British Pounds "+
							convertKnutsToWizardingCurrency(convertWizardingCurrencyToKnuts(galleons,sickles,knuts))+" is GBP"+conversionValue);
				}
				else if (userFunctionSelection == 2) 
				{
					integerRequired = true;
					prompt = "Enter the number of Knuts > ";
					errorMessage = "Error: The number of Knuts should be an integer (e.g. 56)";
					int knuts = (int)(getNumberFromUser( prompt, errorMessage, inputScanner, integerRequired ));
					System.out.print(knuts+" knuts is "+convertKnutsToWizardingCurrency(knuts));
					double conversionValue=(double)(convertWizardingCurrencyToPence(0,0,knuts));
					conversionValue = conversionValue / 100.0;
					System.out.println(" is equal to GBP"+conversionValue);
				}
				else if (userFunctionSelection == 3) 
				{
					integerRequired = false;
					double number = 0;
					prompt = "Enter the number of British pounds and pence (e.g. 1.24) > ";
					errorMessage ="Error: Invalid number of British pounds and pence";
					number = getNumberFromUser( prompt, errorMessage, inputScanner, integerRequired );
					int pounds =(int)(number);
					int pence = (int)(number*100-pounds*100);
					System.out.println("In wizarding currency GBP"+number+" is "+convertGBPToWizardingCurrency(pounds,pence));
				}
			}
			else
				System.out.println(errorMessage);

		} while (userFunctionSelection != 4);
	}

}
