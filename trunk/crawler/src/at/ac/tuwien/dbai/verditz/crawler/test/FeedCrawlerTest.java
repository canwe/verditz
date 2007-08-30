package at.ac.tuwien.dbai.verditz.crawler.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;

import at.ac.tuwien.dbai.verditz.crawler.FeedCrawler;

import com.sun.syndication.feed.synd.SyndFeed;
import com.sun.syndication.fetcher.FetcherEvent;
import com.sun.syndication.fetcher.FetcherListener;

public class FeedCrawlerTest {

	@org.junit.Test
	public void test() throws MalformedURLException {

		// create cache on disk if exists. See DefaultFeedStrategy uses local
		// cache
		File cacheFile = new File("cache");
		if (cacheFile.delete()) {
			System.out.println("Deleted cache file: "
					+ cacheFile.getAbsolutePath());
		}

		// we will register this handler with the crawler
		final int[] eventFiredCount = new int[1];
		final Collection<SyndFeed> newFeeds = new ArrayList<SyndFeed>();
		final FetcherListener handler = new FetcherListener() {
			public void fetcherEvent(FetcherEvent event) {
				System.out.println(event.getEventType());
				if (event.getEventType() == FetcherEvent.EVENT_TYPE_FEED_RETRIEVED) {
					assertTrue(event.getFeed().getEntries().size() > 0);
					System.out.println(event.getFeed().getTitle());
					newFeeds.add(event.getFeed());
				} else if (event.getEventType() == FetcherEvent.EVENT_TYPE_FEED_UNCHANGED) {
					assertNull(event.getFeed());
				}
				eventFiredCount[0]++;
			}
		};

		FeedCrawler crawler = new FeedCrawler(handler);

		// create feed source, contain one remote feed:
		URL testFeed = new URL("http://blog.bookworm.at/feeds/posts/default");
		Collection<URL> feedList = new ArrayList<URL>();
		feedList.add(testFeed);
		crawler.addFeedSource(feedList);

		// first time we fetch: cache is empty, new feed is found
		crawler.fetch();

		// second time: feed is not fetched, because it is in cache.
		crawler.fetch();

		// we should have found exactly one feed
		assertEquals(1, newFeeds.size());

		// 4 events should be fired: FEED_POLLED, FEED_RETRIEVED, FEED_POLLED,
		// FEED_UNCHANGED
		assertEquals(4, eventFiredCount[0]);

	}

}
