package at.ac.tuwien.dbai.crawler;

import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

import org.apache.log4j.Logger;

public final class FeedCrawler {

	private final static Logger log = Logger.getLogger(FeedCrawler.class);

	private final Collection<Iterable<URL>> sources;

	private FetchStrategy distributionStrategy;

	private boolean stopped;

	public FeedCrawler(final Collection<Iterable<URL>> sources,
			final FetchStrategy distributionStrategy) {
		this.distributionStrategy = distributionStrategy;
		this.sources = sources;
	}

	public FeedCrawler(final Collection<Iterable<URL>> sources) {
		this(sources, new DefaultFetchStrategy());
	}

	public FeedCrawler() {
		this(new ArrayList<Iterable<URL>>());
	}

	public void addFeedSource(final Iterable<URL> source) {
		this.sources.add(source);
	}

	public void removeFeedSource(final Iterable<URL> source) {
		this.sources.remove(source);
	}

	public Collection<Iterable<URL>> getSources() {
		return Collections.unmodifiableCollection(sources);
	}

	public void addFetcherEventListener(
			com.sun.syndication.fetcher.FetcherListener observer) {
		this.distributionStrategy.addObserver(observer);
	}

	public void removeFetcherEventListener(
			com.sun.syndication.fetcher.FetcherListener observer) {
		this.distributionStrategy.removeObserver(observer);
	}

	public void fetch() {
		log.debug("fetching feeds...");
		this.distributionStrategy.execute(sources);
		log.debug("fetched all feeds");
	}

	public void start(final int timeout) {
		log.info("starting crawler...");
		stopped = false;

		while (!stopped) {
			fetch();
			log.debug("Waiting");
			try {
				Thread.sleep(timeout);
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
		log.info("stopping crawler...");
	}

	public void stop() {
		this.stopped = true;
	}

	public FetchStrategy getFetchStrategy() {
		return distributionStrategy;
	}

	public void setFetchStrategy(final FetchStrategy distributionStrategy) {
		this.distributionStrategy = distributionStrategy;
	}

}
