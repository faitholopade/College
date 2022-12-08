/*
 * Suppose you are smart enough to choose  different names for your instance variable and  methods arguments.​

Create a class Account with a setData  method that takes parameters and sets the  value of the instance variables.​

Create two objects of the class, each calling the​

setData method.​
 */
public class Account {
	private String name;
	private int accNum;
	
	public void setData(String accName, int accountNum) {
		name = accName;
		accNum = accountNum;
	}
	
	public void showData()
    {
        System.out.println("Name = " + name);
        System.out.println("Value of accNum = " + accNum);
    }

	public static void main(String[] args) {
		Account obj1 = new Account();
		Account obj2 = new Account();
		obj1.setData("Faith", 8956);
		obj2.setData("Amy", 4158);
		obj1.showData();
		obj2.showData();
		

	}

}
