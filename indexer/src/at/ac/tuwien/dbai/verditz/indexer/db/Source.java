package at.ac.tuwien.dbai.verditz.indexer.db;

import java.net.URL;

public class Source {
	private Integer id;

	private URL url;
	
	public Integer getId() {
		return id;
	}

	public URL getUrl() {
		return url;
	}

	public void setUrl(URL url) {
		this.url = url;
	}
}
