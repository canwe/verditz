package at.ac.tuwien.dbai.verditz.crawler;

import java.util.Collection;

import org.apache.log4j.Logger;

import com.sun.syndication.feed.synd.SyndEntry;
import com.sun.syndication.feed.synd.SyndFeed;
import com.sun.syndication.fetcher.FetcherEvent;
import com.sun.syndication.fetcher.FetcherListener;

public class DocumentCrawler implements FetcherListener {

	private static final Logger log = Logger.getLogger(DocumentCrawler.class);

	@SuppressWarnings("unchecked")
	public void fetcherEvent(FetcherEvent event) {
		if (event.getEventType() == FetcherEvent.EVENT_TYPE_FEED_RETRIEVED) {
			final SyndFeed feed = event.getFeed();

			for (final SyndEntry entry : (Collection<SyndEntry>) feed
					.getEntries()) {
				processEntry(entry);
			}
		}
	}

	private void processEntry(final SyndEntry entry) {
		log.info(entry.getTitle());
	}

}
