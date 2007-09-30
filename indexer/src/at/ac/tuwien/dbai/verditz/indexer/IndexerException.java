package at.ac.tuwien.dbai.verditz.indexer;

public class IndexerException extends Exception {

	private static final long serialVersionUID = 8478616003253937723L;

	public IndexerException() {
		super();
	}
	
	public IndexerException(String message) {
		super(message);
	}
	
	public IndexerException(Throwable throwable) {
		super(throwable);
	}
}