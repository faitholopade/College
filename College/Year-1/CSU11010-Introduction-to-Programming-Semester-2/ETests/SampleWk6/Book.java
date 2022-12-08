public class Book
{
	private String isbn;
	private String title;
	private String author;
	private String publisher;
	private int publishYear;
	private int quantity;
	private double price;

	public Book()
	{

	}

	public Book(String new_isbn, String new_title, String new_author, String new_publisher, int new_publishYear, int new_quantity, double new_price)
	{
		isbn = new_isbn;
		title = new_title;
		author = new_author;
		publisher = new_publisher;
		publishYear = new_publishYear;
		quantity = new_quantity;
		price = new_price;
	}

	public int getPublishYear()
	{
		return publishYear;
	}

	public void setPublishYear(int new_publishYear)
	{
		publishYear = new_publishYear;
	}

	public int getQuantity()
	{
		return quantity;
	}

	public void setQuantity(int new_quantity)
	{
		quantity = new_quantity;
	}

	public double getPrice()
	{
		return price;
	}

	public void setPrice(double new_price)
	{
		price = new_price;
	}

	public String getIsbn()
	{
		return isbn;
	}

	public void setIsbn(String new_isbn)
	{
		isbn = new_isbn;
	}

	public String getTitle()
	{
		return title;
	}

	public void setTitle(String new_title)
	{
		title = new_title;
	}

	public String getAuthor()
	{
		return author;
	}

	public void setAuthor(String new_author)
	{
		author = new_author;
	}

	public String getPublisher()
	{
		return publisher;
	}

	public void setPublisher(String new_publisher)
	{
		publisher = new_publisher;
	}

	@Override
	public String toString()
	{
		return "isbn: " + isbn + ", Title: " + title + ", Author: " + author + ", Publisher: " + publisher + ", PublishYear: " + publishYear + ", Quantity: " + quantity + ", Price: " + price + "\n\n";
	}

}