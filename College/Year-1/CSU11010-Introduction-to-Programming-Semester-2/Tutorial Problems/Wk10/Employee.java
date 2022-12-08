
public class Employee extends Person{
	private String jobTitle; 

	public Employee(String firstName, String lastName, String address, int id, String jobTitle)
	{
		super(firstName, lastName, address, id);
		this.jobTitle = jobTitle;
	}
	
	public void setJobTitle(String jB) {
		jobTitle = jB;
	}
	
	public String getJobTitle() {
		return jobTitle;
	}
	
    @Override
    public void display()
    {
        System.out.println(getFirstName() + " " + getLastName() + " "
                           + getAddress() + " " + id + " " + jobTitle); 
    }

	public static void main(String[] args) {
		// TODO Auto-generated method stub

	}

}
