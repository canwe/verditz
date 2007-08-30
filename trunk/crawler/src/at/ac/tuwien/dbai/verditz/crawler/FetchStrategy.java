package at.ac.tuwien.dbai.verditz.crawler;

import java.net.URL;
import java.util.Collection;
import java.util.Iterator;

import com.sun.syndication.fetcher.FetcherListener;

/** This decides how the FeedCrawler fetches Feeds */
public interface FetchStrategy {

	void addObserver(FetcherListener observer);

	void removeObserver(FetcherListener observer);

	void execute(Collection<Iterable<URL>> sources);

}
