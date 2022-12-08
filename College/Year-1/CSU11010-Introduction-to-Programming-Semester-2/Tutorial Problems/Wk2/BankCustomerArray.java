//Update the BankCustomer class creating an array storing data of type bankCustomerArray and add your BankCustomer to this array.
public class BankCustomerArray {
	private int accountNumber;
	private String customerName;
	private String customerAddress;
	private int customerDateOfBirth;
	
	public void setAccountNumber (int customerAccountNumber)
	{
		accountNumber = customerAccountNumber;
		return;
	}

	public void setCustomerName(String name)
	{
		customerName = name;
		return;
	}

	public void setCustomerAddress(String address)
	{
		customerAddress = address;
		return;
	}

	public void setCustomerDateOfBirth(int dateOfBirth)
	{
		customerDateOfBirth = dateOfBirth;
		return;
	}

	public int getAccountNumber()
	{
		return accountNumber;
	}

	public String getCustomerName()
	{
		return customerName;
	}

	public String getCustomerAddress()
	{
		return customerAddress;
	}

	public int getCustomerDateOfBirth()
	{
		return customerDateOfBirth;
	}


	public static void main(String[] args) {
		BankCustomerArray customer1 = new BankCustomerArray();
		customer1.setAccountNumber(315645);
		customer1.setCustomerName("Faith Olopade");
		customer1.setCustomerAddress(" 3 Yellow Brick Road");
		customer1.setCustomerDateOfBirth(20082002);

		/* Create an array that can hold reference to 10
        BankCustomer objects. Does not create the objects */
	BankCustomerArray[] bankCustomers = new BankCustomerArray[10];
	
	// Instantiate the objects
	for(int i=0; i<bankCustomers.length; i++)
	    bankCustomers[i] = new BankCustomerArray();

	bankCustomers[0].setAccountNumber(12345);

	System.out.println("Customer Account Number is " + bankCustomers[0].getAccountNumber());
	}

}
