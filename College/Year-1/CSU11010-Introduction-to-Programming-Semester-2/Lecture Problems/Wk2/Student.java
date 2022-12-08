
public class Student {
	private int age;
	
	public boolean setAge(int studentAge)
	{
		if (studentAge >= 0 && studentAge < 130)
		{
			age = studentAge;
			return true;
		
		}
		return false;
	}
	
	public int getAge()
	{
		return age;
	}
	
	public static void main(String[] args) {
		Student myStudent = new Student();
		myStudent.setAge(10);
		

	}

}