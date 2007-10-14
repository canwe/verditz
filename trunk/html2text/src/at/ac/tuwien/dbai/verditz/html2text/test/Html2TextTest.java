package at.ac.tuwien.dbai.verditz.html2text.test;

import java.io.File;
import java.net.MalformedURLException;

import org.htmlparser.util.ParserException;

import junit.framework.TestCase;
import at.ac.tuwien.dbai.verditz.html2text.Html2Text;

public class Html2TextTest extends TestCase {
	public void testHtml2Text() throws MalformedURLException, Exception {
		String url = new File("data/blog_bookworm_at.html").toURI().toURL()
				.toString();
		String text = Html2Text.getText(url);
		assertTrue(text.contains("blog.bookworm.at"));
		assertTrue(text
				.contains("Benjamin Ferrari's Weblog. This blog is still beta."));
		assertFalse(text.contains("<script type='text/javascript'>"));
		assertFalse(text.contains("<div class='post'>"));
		assertFalse(text.contains("<ul>"));

		url = new File("data/reuters.html").toURI().toURL().toString();
		text = Html2Text.getText(url);
		assertNotNull(text);
		assertFalse(text.contains("document.write"));
		assertTrue(text.contains("WASHINGTON (Reuters)"));
	}

	public void testMalformedUrl() {
		try {
			Html2Text.getText("thisisnourl");
			fail("Html2Text.getText(\"thisisnourl\") should throw an exception.");
		} catch (Exception e) {
		}
	}

	public void testUnknownHost() {
		try {
			Html2Text.getText("http://nonexisting49375.tz");
			fail("Html2Text.getText(\"http://nonexisting49375.tz\") should throw an exception.");
		} catch (Exception e) {
		}
	}
}
