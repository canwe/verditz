package at.ac.tuwien.dbai.verditz.crawler;

import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;

import org.apache.log4j.Logger;

import com.sun.syndication.fetcher.FeedFetcher;
import com.sun.syndication.fetcher.FetcherException;
import com.sun.syndication.fetcher.FetcherListener;
import com.sun.syndication.fetcher.impl.HttpURLFeedFetcher;
import com.sun.syndication.io.FeedException;

public class FeedCrawler {
	private final static Logger log = Logger.getLogger(FeedCrawler.class);

	private Collection<URL> sources = new ArrayList<URL>();
	private Collection<FetcherListener> listeners = new ArrayList<FetcherListener>();

	public FeedCrawler() {
	}

	public FeedCrawler(FetcherListener listener) {
		this.listeners.add(listener);
	}

	public FeedCrawler(FetcherListener listener, Collection<URL> sources) {
		this(listener);
		this.sources = sources;
	}

	public void addFetcherListener(FetcherListener listener) {
		this.listeners.add(listener);
	}
	
	public void addSource(URL source) {
		this.sources.add(source);
	}

	public void fetch() {
		FeedFetcher feedFetcher = new HttpURLFeedFetcher();

		for (FetcherListener listener : this.listeners) {
			feedFetcher.addFetcherEventListener(listener);
		}

		for (URL source : sources) {
			try {
				feedFetcher.retrieveFeed(source);
			} catch (IllegalArgumentException e) {
				log.error(e);
			} catch (IOException e) {
				log.error(e);
			} catch (FeedException e) {
				log.error(e);
			} catch (FetcherException e) {
				log.error(e);
			}
		}
	}
}
