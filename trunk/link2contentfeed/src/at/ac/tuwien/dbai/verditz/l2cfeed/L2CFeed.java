package at.ac.tuwien.dbai.verditz.l2cfeed;

import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import org.apache.log4j.Logger;

import com.sun.syndication.feed.synd.SyndContent;
import com.sun.syndication.feed.synd.SyndContentImpl;
import com.sun.syndication.feed.synd.SyndEntry;
import com.sun.syndication.feed.synd.SyndFeed;

public class L2CFeed {

	private static final Logger log = Logger.getLogger(L2CFeed.class);

	public static SyndFeed link2Content(SyndFeed feed) {
		List<SyndEntry> entries = new ArrayList<SyndEntry>();
		for (SyndEntry entry : (List<SyndEntry>) feed.getEntries()) {
			try {
				URL url = new URL(entry.getLink());
				String text = L2C.getText(url);
				//replacing binary zero because jdom would choke on it
				text = text.replaceAll("\0", "");
				SyndContent content = new SyndContentImpl();
				content.setType("text/plain");
				content.setValue(text);
				entry.setDescription(content);
				entries.add(entry);
			} catch (Exception e) {
				log.error("error getting text for url:" + entry.getLink(), e);
			}

		}
		feed.setEntries(entries);
		return feed;
	}

}
