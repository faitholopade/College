/*
 *  Create a Java class called BankCustomer with the following variables: accountNumber, customerName, customerAddress, customerDateOfBirth
For each of the variables, write methods to set and get the variables
Create a BankCustomer object and initialize the variables
 */
public class BankCustomer {
	public int accountNumber;
	public String customerName;
	public String customerAddress;
	public int customerDateOfBirth;

	public void setAcc(int acc) {
		accountNumber = acc;
	}

	public void setName(String name) {
		customerName = name;
	}

	public void setAdd(String add) {
		customerAddress = add;
	}

	public void setDob(int dob) {		
		customerDateOfBirth = dob;
	}

	public String getName() {
		return customerName;
	}

	public String getAdd() {
		return customerAddress;
	}

	public int getDob() {
		return customerDateOfBirth;
	}

	public int getAcc() {
		return accountNumber;
	}

	public String toString() {
		return "Name" + customerName + "Account Number" + accountNumber + "Address" + customerAddress + "DOB" + customerDateOfBirth;
	}

	public static void main(String[] args) {
		BankCustomer customer1 = new BankCustomer();
		customer1.setAcc(21345);
		customer1.setAdd("3 Yellow Brick Road");
		customer1.setName("Amy Ryan");
		customer1.setDob(20082003);

		customer1.getAcc();
		customer1.getAdd();
		customer1.getDob();
		customer1.getName();




	}

}
