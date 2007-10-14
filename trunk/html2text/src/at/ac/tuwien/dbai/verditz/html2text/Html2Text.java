package at.ac.tuwien.dbai.verditz.html2text;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.URL;

import org.htmlparser.NodeFilter;
import org.htmlparser.Parser;
import org.htmlparser.filters.AndFilter;
import org.htmlparser.filters.HasParentFilter;
import org.htmlparser.filters.NodeClassFilter;
import org.htmlparser.filters.NotFilter;
import org.htmlparser.filters.TagNameFilter;
import org.htmlparser.nodes.TextNode;
import org.htmlparser.util.NodeList;

public class Html2Text {

	public static String getText(String url) throws Exception {
		NodeFilter textNodeFilter = new NodeClassFilter(TextNode.class);
		NodeFilter scriptParentFilter = new NotFilter(new HasParentFilter(new TagNameFilter("SCRIPT"), true));
		NodeFilter styleParentFilter = new NotFilter(new HasParentFilter(new TagNameFilter("STYLE"), true));
		NodeFilter parentFilter = new AndFilter(scriptParentFilter, styleParentFilter);
		NodeFilter filter = new AndFilter(textNodeFilter, parentFilter);
		
		// WORKAROUND: we need to replace all scripts from the page with a regexp
		//             because htmlparser doesn't filter scripts (although the
		//             filter is defined above) if the script isn't embedded in
		//             a html comment section and contains html markup. (i.e.
		//             document.write("<b>hello world</b>");)
		URL myUrl = new URL(url);
		BufferedReader in = new BufferedReader(
					new InputStreamReader(
					myUrl.openStream()));

		StringBuilder builder = new StringBuilder();
		String inputLine;
		while ((inputLine = in.readLine()) != null) {
		    builder.append(inputLine);
		}

		in.close();
		
		String content = builder.toString();
		content = content.replaceAll("<script.*?</script>", "");
		
		Parser parser = new Parser();
		parser.setInputHTML(content);
		// END WORKAROUND
		
		NodeList list = parser.parse(filter);
		return list.toHtml();
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
