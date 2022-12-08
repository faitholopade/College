
public abstract class ConnectPlayer {

	public int playerId;

	ConnectPlayer(int playerId) {
		this.setPlayerId(playerId);
	}

	public abstract int playerMove(Connect4Grid2DArray connect4Grid);

	public int getPlayerId() {
		return playerId;
	}

	public void setPlayerId(int playerId) {
		this.playerId = playerId;
	}

	public static void main(String[] args) {
		// TODO Auto-generated method stub

	}

	//	protected abstract int playerMove();

}
