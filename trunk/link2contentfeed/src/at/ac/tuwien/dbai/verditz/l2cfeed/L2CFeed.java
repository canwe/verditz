package at.ac.tuwien.dbai.verditz.l2cfeed;

import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import org.apache.log4j.Logger;

import com.sun.syndication.feed.synd.SyndContent;
import com.sun.syndication.feed.synd.SyndContentImpl;
import com.sun.syndication.feed.synd.SyndEntry;
import com.sun.syndication.feed.synd.SyndFeed;

public class L2CFeed {

	private static final Logger log = Logger.getLogger(L2CFeed.class);
	private static final int MAX_CONCURRENT_THREADS = 3;

	private static List<List<SyndEntry>> getSubLists(List<SyndEntry> entries) {
		List<List<SyndEntry>> entryLists = new ArrayList<List<SyndEntry>>();
		for (int i=0;i<entries.size();i=i+MAX_CONCURRENT_THREADS) {
			List<SyndEntry> subList;
			if (i + MAX_CONCURRENT_THREADS < entries.size())
				subList = entries.subList(i, i + MAX_CONCURRENT_THREADS);
			else
				subList = entries.subList(i, entries.size());
			entryLists.add(subList);
		}
		return entryLists;
	}

	public static SyndFeed link2Content(SyndFeed feed) {
		List<List<SyndEntry>> entryLists = getSubLists(feed.getEntries());
		List<SyndEntry> entries = new ArrayList<SyndEntry>();
		for (List<SyndEntry> entryList : entryLists) {
			Map<SyndEntry, Future<String>> results = new HashMap<SyndEntry, Future<String>>();
			for (SyndEntry entry : entryList) {
				log.info("getting plaintext for entry: " + entry.getLink());
				try {
					URL url = new URL(entry.getLink());
					ExecutorService service = Executors.newFixedThreadPool(feed
							.getEntries().size());
					Future<String> future = service.submit(new L2C(url));
					results.put(entry, future);
				} catch (Exception e) {
					log.error(e);
				}
			}

			for (SyndEntry entry : results.keySet()) {
				String text;
				try {
					text = results.get(entry).get();
					SyndContent content = new SyndContentImpl();
					content.setType("text/plain");
					content.setValue(text);
					entry.setDescription(content);
					entries.add(entry);
				} catch (Exception e) {
					log.error("error getting text for url:" + entry.getLink(),
							e);
				}

			}
		}
		feed.setEntries(entries);
		return feed;
	}

}
