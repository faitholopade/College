
public class PieceWorker extends Employee{

	private double wage;
	private int pieces;

	public PieceWorker(String firstName, String lastName, String socialSecurityNumber, double wage, int pieces) {
		super(firstName, lastName, socialSecurityNumber);
		if (wage < 0.0)
			throw new IllegalArgumentException(
					"Hourly wage must be >= 0.0");

		if ((pieces < 0.0) || (pieces> 168.0))
			throw new IllegalArgumentException(
					"Pieces made must be >= 0.0");

		this.wage = wage;
		this.pieces = pieces;
	}

	public static void main(String[] args) {

	}

	public void setWage(double wage) {
		if (wage < 0.0) 
			throw new IllegalArgumentException(
					"Hourly wage must be >= 0.0");
		this.wage = wage;
	}

	public int getPieces() {
		return pieces;
	}

	public void setPieces(int piece) {
		if ((pieces < 0.0) || (pieces > 168.0)) // validate hours
			throw new IllegalArgumentException(
					"Pieces made must be >= 0.0");
		this.pieces = piece;
	}

	public double getWage() {
		return wage;
	}

	@Override
	public double earnings() {
		return getWage() * getPieces(); 
	}

	@Override                                                             
	public String toString()                                              
	{                                                                     
		return String.format("Piece Woker: %s%n%s: $%,.2f; %s: %,.2f",
				super.toString(), "hourly wage", getWage(),                     
				"pieces made", getPieces());                                   
	}  
}
