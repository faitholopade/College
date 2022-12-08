/*
 * You are required to make use of the following files/classes (which can be found on
Blackboard) in your program:
1) books.txt; 2) Book.java; 3) ChargeCard.java; 4) OnlineBookStore.java
Develop a program that allows books to be purchased from an online book store by updating
OnlineBookStore.java. The program currently reads books from file and stores them in a
collection. You are required to update the program to 1) present all the book details to the
user; 2) get the book that the user in interested in; 3) allow the user to top-up his/her charge
card; and 4) allow a user to purchase books one at a time. To develop this program, you are
required to update OnlineBookStore.java with the following methods (do not alter the
mainline):
1. public static void printBookDetails(ArrayList<Book> bookList)
This method is required to print (to the console) the details (isbn, title, author,
publisher, publish year, quantity and price) of each book in bookList.
2. public static Book getBook(ArrayList<Book> bookList, String
title)
This method is required to return the Book object which has a title that matches the
title provided, otherwise it returns null. You can assume the titles in bookList are
unique.
3. public static void topUpCard(ChargeCard card, double amount)
This method is required to top-up the charge card with the amount specified.
4. public static void purchaseBook(ArrayList<Book> bookList)
This method is required to:
a) Ask the user to enter the funds (positive number) in his/her card. Use this amount
to create and top-up a ChargeCard object.
b) Within a loop, ask the user to enter the book title he/she would like to purchase. Get
the Book from the collection that matches this title. Ensure there is at least one copy
of this book and sufficient funds in the card. What should happen to the quantity
variable and the funds when a book has been purchased? Allow the user to make
more purchases or quit.
c) Display all error and success messages to the user for (a) and (b) above.
 */
import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Scanner;

public class OnlineBookStore
{
	public static int ISBN_INDEX = 0;
	public static int TITLE_INDEX = 1;
	public static int AUTHOR_INDEX = 2;
	public static int PUBLISHER_INDEX = 3;
	public static int PUBLISHER_YEAR_INDEX = 4;
	public static int QUANTITY_INDEX = 5;
	public static int PRICE_INDEX = 6;

	public static void main(String[] args)
	{
		ArrayList<Book> bookList = new ArrayList<Book>();
		try
		{
			FileReader fileReader = new FileReader("books.txt");// Enter the entire path of the file if needed
			BufferedReader bufferedReader = new BufferedReader(fileReader);  
			boolean endOfFile = false;

			while(!endOfFile)
			{
				String bookLine = bufferedReader.readLine();
				if (bookLine != null)
				{
					String[] bookData = bookLine.split(", ");
					String isbn = bookData[ISBN_INDEX];
					String title = bookData[TITLE_INDEX];
					String author = bookData[AUTHOR_INDEX];
					String publisher = bookData[PUBLISHER_INDEX];
					int publishYear = Integer.parseInt (bookData[PUBLISHER_YEAR_INDEX]);
					int quantity = Integer.parseInt (bookData[QUANTITY_INDEX]);
					double price = Double.parseDouble (bookData[PRICE_INDEX]);
					Book book = new Book(isbn, title, author, publisher, publishYear, quantity, price);
					bookList.add(book);

				}
				else
				{
					endOfFile = true;
				}
			}
			bufferedReader.close();    
			fileReader.close();
		} // End try 

		catch (FileNotFoundException e)
		{
			e.printStackTrace();
		}
		catch (IOException e)
		{
			e.printStackTrace();
		}

		// Uncomment the following lines once you have implemented the required methods
		printBookDetails(bookList);
		purchaseBook(bookList);
	}


	public static void printBookDetails(ArrayList<Book> bookList) {
		for(int i = 0; i < bookList.size(); i++) {
			Book book = bookList.get(i);
			System.out.println(book);

		}
	}

	public static Book getBook(ArrayList<Book> bookList, String title) {

		Book book;

		for(int i = 0; i < bookList.size(); i++) {
			book = bookList.get(i);
			if (book.getTitle().equals(title)) {

				return book;

			}

		}

		return null;

	}

	public static void topUpCard(ChargeCard card, double amount) {

		card.topUpFunds(amount);

	}

	public static void purchaseBook(ArrayList<Book> bookList) {

		String title = " ";
		Scanner userInput = new Scanner(System.in);
		Scanner userInput2 = new Scanner(System.in);


		System.out.println("Enter an amount: ");
		double amount = userInput.nextDouble();
		ChargeCard card = new ChargeCard();
		topUpCard(card, amount);

		boolean finished =  false;

		while(!finished){

			System.out.println("Which book?");
			title = userInput2.nextLine();

			Book book1 = getBook(bookList, title);


			if (book1.getQuantity() > 0 && card.getFunds() > 0) {

				for(int i = 0; i < bookList.size(); i++) {

					Book book = bookList.get(i);

					if ( book == bookList.get(i)) {

						bookList.get(i).setQuantity(bookList.get(i).getQuantity() - 1); 

					}

				}

				System.out.println("Book purchased.");
			}

			printBookDetails(bookList);






		}

	}	

}