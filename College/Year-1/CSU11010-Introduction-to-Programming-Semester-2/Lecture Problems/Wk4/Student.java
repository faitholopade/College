/*
 * Write a program to print the names of students by  creating a Student class.​

If no name is passed while creating an object of​

Student class, then the name should be “Unknown”.​

Otherwise, the name should be equal to the String​

value passed while creating object of Student class.
 */
public class Student
{
	private String name;

	public Student(String s)
	{
		name = s;
	}

	public Student()
	{
		name = "Unknown";
	}

	public void printName()
	{
		System.out.println(name);
	}
}

