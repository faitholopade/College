
public class Student extends Person{
	private double gpa;

	public Student(String firstName, String lastName, String address, int id, double gpa)
	{
		super(firstName, lastName, address, id);
		this.gpa = gpa;
	}

	public void setGpa(double GPA) {
		gpa = GPA;
	}

	public double getGpa() {
		return gpa;
	}

	@Override
	public void display()
	{
		System.out.println(getFirstName() + " " + getLastName() + " "
				+ getAddress() + " " + id + " " + gpa); 
	}

	public static void main(String[] args) {
		// TODO Auto-generated method stub

	}

}
