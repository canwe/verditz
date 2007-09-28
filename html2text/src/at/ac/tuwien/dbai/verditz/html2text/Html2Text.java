package at.ac.tuwien.dbai.verditz.html2text;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;

import org.htmlparser.beans.StringBean;

public class Html2Text {

	public static String getText(String url) {
		StringBean sb = new StringBean();
		sb.setLinks(false);
		try {
			URL myUrl = new URL(url);
			URLConnection connection = myUrl.openConnection();
			// StringBean swallows all exceptions so we try to get an
			// inputstream
			// because this would throw an exception if the host is unknown
			connection.getInputStream();
			sb.setConnection(connection);
			return sb.getStrings();
		} catch (MalformedURLException e) {
			throw new RuntimeException(e);
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

	public static void main(String[] args) {
		if (args.length == 0) {
			System.err.println("Usage: Html2Text <URL>");
			System.exit(1);
		}
		try {
			String text = Html2Text.getText(args[0]);
			System.out.println(text);
		} catch (RuntimeException e) {
			System.err.println("Either malformed URL or host is unreachable.");
		}
	}
}
