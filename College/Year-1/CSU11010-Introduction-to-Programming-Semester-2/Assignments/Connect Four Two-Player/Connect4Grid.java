
public interface Connect4Grid {
	public abstract void emptyGrid();
	public abstract String toString();
	public abstract boolean isValidColumn(int column);
	public abstract boolean isColumnFull(int column);
	public abstract void dropPiece(ConnectPlayer player, int column);
	public abstract boolean didLastPieceConnect4();
	public abstract boolean isGridFull();
	//boolean didLastPieceConnect4();


}
