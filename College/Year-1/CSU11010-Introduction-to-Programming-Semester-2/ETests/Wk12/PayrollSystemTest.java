
public class PayrollSystemTest {

	public static void main(String[] args) {

		Employee [] employees = new Employee[4];

		SalariedEmployee employee1  = new SalariedEmployee("John", "Smith", "111-11-1111", 800.00);
		System.out.println(employee1);
		System.out.println("First Name: "+employee1.getFirstName());
		System.out.println("Last Name: "+employee1.getLastName());
		System.out.println("Social Security Number: " +employee1.getSocialSecurityNumber());
		System.out.println("Weekly Salary: "+employee1.getWeeklySalary());
		System.out.println("Earnings: "+employee1.earnings());

		HourlyEmployee employee2 = new HourlyEmployee("Karen", "Price", "222-22-2222", 16.75, 40.00);
		System.out.println(employee2);
		System.out.println("First Name: "+employee2.getFirstName());
		System.out.println("Last Name: "+employee2.getLastName());
		System.out.println("Social Security Number: "+employee2.getSocialSecurityNumber());
		System.out.println("Hourly Wage: " +employee2.getWage());
		System.out.println("Hours Worked: "+employee2.getHours());
		System.out.println("Earnings: "+employee2.earnings());

		CommissionEmployee employee3 = new CommissionEmployee("Sue", "Jones", "333-33-3333", 10000.00, 0.06);
		System.out.println(employee3);
		System.out.println("First Name: "+employee3.getFirstName());
		System.out.println("Last Name: "+employee3.getLastName());
		System.out.println("Social Security Number: " +employee3.getSocialSecurityNumber());
		System.out.println("Gross Sales: "+employee3.getGrossSales());
		System.out.println("Commission Rate: "+employee3.getCommissionRate());
		System.out.println("Earnings: "+employee3.earnings());

		BasePlusCommissionEmployee employee4 = new BasePlusCommissionEmployee("Bob", "Lewis", "444-44-4444", 5000.00, 0.04, 300.00);
		System.out.println(employee4);
		System.out.println("First Name: "+employee4.getFirstName());
		System.out.println("Last Name: "+employee4.getLastName());
		System.out.println("Social Security Number: " +employee4.getSocialSecurityNumber());
		System.out.println("Gross Sales: "+employee4.getGrossSales());
		System.out.println("Commission Rate: "+employee4.getCommissionRate());
		System.out.println("Base Salary: "+employee4.getBaseSalary());
		System.out.println("Earnings: "+employee4.earnings());

		PieceWorker employee5 = new PieceWorker("Rick", "Bridges", "555-55-5555", 2.25, 400);
		System.out.println(employee5);
		System.out.println("First Name: "+employee5.getFirstName());
		System.out.println("Last Name: "+employee5.getLastName());
		System.out.println("Social Security Number: " +employee5.getSocialSecurityNumber());
		System.out.println("Wage per piece: "+employee5.getWage());
		System.out.println("Pieces produced: "+employee5.getPieces());
		System.out.println("Earnings: "+employee5.earnings());

		employees[0] = employee1;
		employees[1] = employee2;
		employees[2] = employee3;
		employees[3] = employee4;
		employees[4] = employee5;


	}

}