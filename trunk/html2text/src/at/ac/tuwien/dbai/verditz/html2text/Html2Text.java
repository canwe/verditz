package at.ac.tuwien.dbai.verditz.html2text;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLConnection;
import java.util.regex.Pattern;

import org.htmlparser.NodeFilter;
import org.htmlparser.Parser;
import org.htmlparser.filters.AndFilter;
import org.htmlparser.filters.HasParentFilter;
import org.htmlparser.filters.NodeClassFilter;
import org.htmlparser.filters.NotFilter;
import org.htmlparser.filters.TagNameFilter;
import org.htmlparser.nodes.TextNode;
import org.htmlparser.util.NodeList;
import org.htmlparser.util.ParserException;
import org.htmlparser.util.Translate;

public class Html2Text {
	
	private static String getUrlContent(String url) throws IOException {
		return Html2Text.getUrlContent(url, 5000);
	}

	private static String getUrlContent(String url, int timeout) throws IOException {
		URL myUrl = new URL(url);
		URLConnection connection = myUrl.openConnection();
		connection.setConnectTimeout(timeout);

		String charset;
		String[] contentTypeKeyMaps = connection.getContentType().split(";");
		if (contentTypeKeyMaps.length > 1 && contentTypeKeyMaps[1].split("=").length > 1) {
			charset = contentTypeKeyMaps[1].split("=")[1].trim();
			// some servers give the name of the encoding enclosed in quotes
			// (i.e. charset="UTF-8") which is not standard HTTP (see
			// http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html (14.17))
			// we want to be tolerant and fetch these resources anyway
			charset = charset.replaceAll("[\"']", "");
		} else {
			charset = "ISO-8859-1";
		}

		BufferedReader in = new BufferedReader(new InputStreamReader(connection
				.getInputStream(), charset));

		StringBuilder builder = new StringBuilder();
		String inputLine;
		while ((inputLine = in.readLine()) != null) {
			builder.append(inputLine + System.getProperty("line.separator"));
		}

		in.close();

		String content = builder.toString();
		return content;
	}

	private static String html2Text(String html) throws ParserException {
		Parser parser = new Parser();
		parser.setInputHTML(html);

		NodeFilter textNodeFilter = new NodeClassFilter(TextNode.class);
		NodeFilter scriptParentFilter = new NotFilter(new HasParentFilter(
				new TagNameFilter("SCRIPT"), true));
		NodeFilter styleParentFilter = new NotFilter(new HasParentFilter(
				new TagNameFilter("STYLE"), true));
		NodeFilter parentFilter = new AndFilter(scriptParentFilter,
				styleParentFilter);
		NodeFilter filter = new AndFilter(textNodeFilter, parentFilter);
		NodeList list = parser.parse(filter);

		StringBuilder plaintext = new StringBuilder();
		for (int i = 0; i < list.size(); i++) {
			TextNode node = (TextNode) list.elementAt(i);
			// appending whitespace because something HTML like:
			// <p>paragraph1</p><p>paragraph2</p>
			// would otherwise result in text: paragraph1paragraph2
			plaintext.append(node.toHtml() + " ");
		}
		return Translate.decode(plaintext.toString());
	}

	public static String getText(String url) throws Exception {
		String content = Html2Text.getUrlContent(url);

		// WORKAROUND: we need to replace all scripts from the page with a
		// regexp
		// because htmlparser doesn't filter scripts (although the
		// filter is defined above) if the script isn't embedded in
		// a html comment section and contains html markup. (i.e.
		// document.write("<b>hello world</b>");)
		content = Pattern.compile("<script.*?</script>", Pattern.CASE_INSENSITIVE | Pattern.DOTALL).matcher(content).replaceAll("");

		return Html2Text.html2Text(content);
	}

	public static void main(String[] args) {
		if (args.length == 0) {
			System.err.println("Usage: Html2Text <URL>");
			System.exit(1);
		}
		try {
			String text = Html2Text.getText(args[0]);
			System.out.println(text);
		} catch (Exception e) {
			System.err.println("Either malformed URL or host is unreachable.");
		}
	}
}
