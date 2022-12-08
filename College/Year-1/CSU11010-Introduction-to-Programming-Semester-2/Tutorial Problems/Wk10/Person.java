/*
 * Write Java code to implement the following inheritance hierarchy. The two subclasses Student and Employee should override the display()
method. In Student, display() should show GPA and the other attributes in the super class, and in Employee, display() should show
the jobTitle and the other attributes in the super class.
 */
public class Person {
	private String firstName;
	private String lastName;
	private String address;
	protected int id;
	
	Person(){
		
	}
	
	Person(String firstName, String lastName, String address, int id){
		this.firstName = firstName;
		this.lastName = lastName;
		this.address = address;
		this.id = id;
	}

	public void setFirstName(String fN) {
		firstName = fN;
	}
	
	public void setLastName(String lN) {
		lastName = lN;
	}
	
	public void setAddress(String add) {
		address = add;
	}
	
	public void setId(int ID) {
		id = ID;
	}
	
	public String getFirstName() {
		return firstName;
	}
	
	public String getLastName() {
		return lastName;
	}
	
	public String getAddress() {
		return address;
	}
	
	public int getId() {
		return id;
	}
	

    public void display()
    {
	System.out.println(firstName + " " + lastName + " " + address + " " + id);
    }
    
	public static void main(String[] args) {
		// TODO Auto-generated method stub

	}

}
