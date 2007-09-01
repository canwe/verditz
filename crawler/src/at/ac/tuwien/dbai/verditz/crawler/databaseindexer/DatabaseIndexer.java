package at.ac.tuwien.dbai.verditz.crawler.databaseindexer;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.apache.log4j.Logger;

import com.sun.syndication.feed.synd.SyndContent;
import com.sun.syndication.feed.synd.SyndEntry;
import com.sun.syndication.feed.synd.SyndFeed;
import com.sun.syndication.fetcher.FetcherEvent;
import com.sun.syndication.fetcher.FetcherListener;

/**
 * This is an Observer that gets notified each time I cross a new feed entry. If
 * that happens, the entry is pumped into a database.
 */
public final class DatabaseIndexer implements FetcherListener {

	final Logger log = Logger.getLogger(DatabaseIndexer.class);

	private final DocumentDatabase database;

	public DatabaseIndexer(final Map<String, String> dbsettings) {
		this.database = new DocumentDatabase(dbsettings);
	}

	public DatabaseIndexer(final String jdbcUrl, final String username,
			final String password, final String driver) {
		this.database = new DocumentDatabase(driver, jdbcUrl, username,
				password);
	}

	@SuppressWarnings("unchecked")
	@Override
	public void fetcherEvent(FetcherEvent event) {
		if (event.getEventType().equals(FetcherEvent.EVENT_TYPE_FEED_RETRIEVED)) {
			SyndFeed feed = event.getFeed();

			Collection<SyndEntry> entries = feed.getEntries();
			for (SyndEntry entry : entries) {
				this.addEntryToDatabse(entry);
			}

		}
	}

	@SuppressWarnings("unchecked")
	private void addEntryToDatabse(final SyndEntry entry) {
		String source = entry.getLink();
		String title = entry.getTitle();
		String body = this.getFeedBody(entry.getContents());
		if (title == null || body == null || source == null) {
			log.info("could not index " + title);
			return;
		}
		this.database.addDocument(title, body, source);
	}

	private String getFeedBody(List<SyndContent> contents) {
		final StringBuilder sb = new StringBuilder();
		for (SyndContent content : contents) {
			if (this.isSupportedContentType(content.getType())) {
				sb.append(content.getValue());
			}
		}
		return sb.toString();
	}

	private boolean isSupportedContentType(final String type) {
		Collection<String> supportedContentTypes = Arrays.asList(new String[] {
				"text/plain", "text/html", "text", "html" });

		return supportedContentTypes.contains(type);
	}
}
