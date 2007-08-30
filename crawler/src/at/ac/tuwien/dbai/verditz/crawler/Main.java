package at.ac.tuwien.dbai.verditz.crawler;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Collection;
import java.util.LinkedList;
import java.util.Scanner;

import org.apache.log4j.Logger;

import at.ac.tuwien.dbai.verditz.crawler.FeedCrawler;

import com.sun.syndication.fetcher.FetcherEvent;
import com.sun.syndication.fetcher.FetcherListener;

public class Main {

	private static final Logger log = Logger.getLogger(Main.class);

	public static void main(String[] args) {
		final Scanner scanner = new Scanner(System.in);

		final Collection<URL> urls = new LinkedList<URL>();

		while (scanner.hasNext()) {
			try {
				urls.add(new URL(scanner.nextLine().trim()));
			} catch (MalformedURLException e) {
				log.info(e.getMessage());
			}
		}
		FeedCrawler crawler = new FeedCrawler(new FetcherListener() {

			public void fetcherEvent(final FetcherEvent event) {
				if (event.getEventType() == FetcherEvent.EVENT_TYPE_FEED_RETRIEVED) {
					System.out.println(event.getFeed());
					log.info(event.getFeed());
				}
			}

		});
		crawler.addFeedSource(urls);

		crawler.fetch();

	}

}
 