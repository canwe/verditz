package at.ac.tuwien.dbai.verditz.indexer.crawler;

import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URL;

public class RedirectResolver {
	public static URL resolve(URL url) throws IOException {
		HttpURLConnection.setFollowRedirects(false);
		HttpURLConnection conn = (HttpURLConnection) url.openConnection();
		conn.setRequestProperty("User-Agent", "Mozilla/5.0");
		String redirect = conn.getHeaderField("Location");
		if (redirect != null) {
			return new URL(redirect);
		} else if (conn.getResponseCode() == HttpURLConnection.HTTP_OK) {
			return url;
		} else {
			throw new IOException(url.toString() + " unavailable");
		}
	}
}
