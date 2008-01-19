package at.ac.tuwien.dbai.verditz.l2cfeed;

import java.net.URL;
import java.net.URLConnection;
import java.util.Arrays;
import java.util.Collection;

import at.ac.tuwien.dbai.verditz.html2text.Html2Text;

public class L2C {

	private static boolean isSupportedContentType(final String type) {
		Collection<String> supportedContentTypes = Arrays.asList(new String[] {
				"text/plain", "text/html", "text", "html", "application/xhtml+xml" });

		return supportedContentTypes.contains(type);
	}

	public static String getText(URL url) throws Exception {
		URLConnection connection = url.openConnection();
		connection.setConnectTimeout(5000);
		if (connection.getContentType() == null) {
			throw new Exception("content type could not be determined");
		}
		String contentType = connection.getContentType().split(";")[0].trim();

		if (!L2C.isSupportedContentType(contentType))
			throw new Exception("content type not supported: " + contentType);
		return Html2Text.getText(url.toString());
	}
}
