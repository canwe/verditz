package at.ac.tuwien.dbai.verditz.l2cfeed;

import java.net.URL;
import java.net.URLConnection;
import java.util.Arrays;
import java.util.Collection;
import java.util.concurrent.Callable;

import at.ac.tuwien.dbai.verditz.html2text.Html2Text;

public class L2C implements Callable<String> {

	private URL url;

	public L2C(URL url) {
		this.url = url;
	}

	private static boolean isSupportedContentType(final String type) {
		Collection<String> supportedContentTypes = Arrays.asList(new String[] {
				"text/plain", "text/html", "text", "html" });

		return supportedContentTypes.contains(type);
	}

	public String call() throws Exception {
		URLConnection connection = url.openConnection();
		if (connection.getContentType() == null) {
			throw new Exception("content type could not be determinded");
		}
		String contentType = connection.getContentType().split(";")[0].trim();
			
		if (!L2C.isSupportedContentType(contentType))
			throw new Exception("content type not supported: " + contentType);
		return Html2Text.getText(this.url.toString());
	}
}
