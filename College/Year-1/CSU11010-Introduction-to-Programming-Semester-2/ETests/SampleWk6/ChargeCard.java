public class ChargeCard
{

	private double funds;

	public ChargeCard()
	{
		funds = 0.0;
	}

	public void topUpFunds(double new_funds)
	{
		funds += new_funds;
	}

	public void removeFunds(double new_funds)
	{
		funds -= new_funds;
	}

	public double getFunds()
	{
		return funds;
	}

	public void setFunds(double new_funds)
	{
		funds = new_funds;
	}
}