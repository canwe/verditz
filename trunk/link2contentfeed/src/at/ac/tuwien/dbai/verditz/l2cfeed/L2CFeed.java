package at.ac.tuwien.dbai.verditz.l2cfeed;

import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutionException;
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
	
	public static SyndFeed link2Content(SyndFeed feed) {
		Map<SyndEntry, Future<String>> results = new HashMap<SyndEntry, 
		Future<String>>();
		for (SyndEntry entry : (List<SyndEntry>) feed.getEntries()) {
			log.info("getting plaintext for entry: "
					+ entry.getLink());
			try {
				URL url = new URL(entry.getLink());
				ExecutorService service = Executors.newFixedThreadPool(feed.getEntries().size());
				Future<String> future = service.submit(new L2C(url));
				results.put(entry, future);
			} catch (Exception e) {
				log.error(e);
			}
		}
		
		for (SyndEntry entry : (List<SyndEntry>) feed.getEntries()) {
			String text;
			try {
				text = results.get(entry).get();
				SyndContent content = new SyndContentImpl();
				content.setType("text/plain");
				content.setValue(text);
				List<SyndContent> contents = new ArrayList<SyndContent>();
				contents.add(content);
				entry.setContents(contents);
			} catch (Exception e) {
				log.error("error getting text for url:" + entry.getLink(), e);
			}
			
		}
		
		return feed;
	}
	
	

}
