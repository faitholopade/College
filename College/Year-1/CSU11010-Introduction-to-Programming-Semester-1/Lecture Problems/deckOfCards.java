
public class deckOfCards {
	private static void randomiseOrder(int[] deck) {
		// TODO Auto-generated method stub
		
	}

	public static void main(String[] args) {
		/*Write code to initialise and shuffle a deck of cards, where
		each card is represented just by its value (not its suit).
		*/
		final String[] CARD_TYPES = { "2", "3", "4", "5",
		"6", "7", "8", "9", "10",
		"Jack", "Queen", "King", "Ace" };
		final int NUMBER_OF_CARD_TYPES = CARD_TYPES.length;
		final int SUITS_IN_DECK = 4;
		final int CARDS_IN_DECK = NUMBER_OF_CARD_TYPES *
		SUITS_IN_DECK;
		int[] deck = new int[CARDS_IN_DECK];
		for (int card=0; card<CARDS_IN_DECK; card++)
		deck[card] = card%NUMBER_OF_CARD_TYPES;
		randomiseOrder( deck );

	}

	

}
