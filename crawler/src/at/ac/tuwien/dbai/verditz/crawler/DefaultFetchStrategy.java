package at.ac.tuwien.dbai.verditz.crawler;

import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import org.apache.log4j.Logger;

import com.sun.syndication.fetcher.FeedFetcher;
import com.sun.syndication.fetcher.FetcherException;
import com.sun.syndication.fetcher.FetcherListener;
import com.sun.syndication.fetcher.impl.FeedFetcherCache;
import com.sun.syndication.fetcher.impl.HttpURLFeedFetcher;
import com.sun.syndication.io.FeedException;

/**
 * The Default Strategy is used by the FeedCrawler if no other Strategy was
 * specified. It can distribute feed parsing over various Threads, but the empty
 * constructor used by the FeedCrawler uses only a single Thread.
 */
public final class DefaultFetchStrategy implements FetchStrategy {

	private final class CircularBuffer implements Iterable<FeedFetcher> {

		private Collection<FeedFetcher> fetchers;

		private Iterator<FeedFetcher> iterator;

		public CircularBuffer(final Collection<FeedFetcher> fetchers) {
			this.fetchers = fetchers;
		}

		public Iterator<FeedFetcher> iterator() {
			this.iterator = fetchers.iterator();
			return new Iterator<FeedFetcher>() {

				public boolean hasNext() {
					return true;
				}

				public FeedFetcher next() {
					if (!iterator.hasNext()) {
						iterator = fetchers.iterator();
					}
					return iterator.next();
				}

				public void remove() {

				}

			};

		}

	}

	private static Logger log = Logger.getLogger(DefaultFetchStrategy.class);

	private final FilePersistentCache cache;

	private Collection<FeedFetcher> fetchers;

	public DefaultFetchStrategy() {
		this.cache = new FilePersistentCache();
		this.fetchers = createFeedFetchers(1, this.cache);
	}

	public DefaultFetchStrategy(int numThreads) {
		this.cache = new FilePersistentCache();
		this.fetchers = createFeedFetchers(numThreads, this.cache);
	}

	public DefaultFetchStrategy(List<FetcherListener> obervers) {
		this.cache = new FilePersistentCache();
	}

	public void addObserver(FetcherListener observer) {
		for (FeedFetcher fetcher : fetchers) {
			fetcher.addFetcherEventListener(observer);
		}
	}

	public void execute(final Collection<Iterable<URL>> sources) {

		final Iterator<FeedFetcher> fetcherPool = new CircularBuffer(fetchers)
				.iterator();

		final ThreadFactory threadFactory = new ThreadFactory(fetchers.size());

		for (final Iterable<URL> source : sources) {
			for (final URL url : source) {
				try {

					threadFactory.newThread(new Runnable() {
						public void run() {
							try {
								fetcherPool.next().retrieveFeed(url);
								cache.saveCache();
							} catch (IllegalArgumentException e) {
								log.info(e);
							} catch (IOException e) {
								log.info(e);
							} catch (FeedException e) {
								log.info(e);
							} catch (FetcherException e) {
								log.info(e);
							}
						}
					}).start();

				} catch (IllegalArgumentException e) {
					log.info(e);
				}

				try {
					Thread.sleep(10);
				} catch (InterruptedException e) {
					e.printStackTrace();
				}

			}

		}

		for (Thread thread : threadFactory.getThreads()) {
			try {
				thread.join();
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}

	}

	public void removeObserver(FetcherListener observer) {
		for (FeedFetcher fetcher : fetchers) {
			fetcher.removeFetcherEventListener(observer);
		}
	}

	private Collection<FeedFetcher> createFeedFetchers(final Integer num,
			FeedFetcherCache cache) {
		Collection<com.sun.syndication.fetcher.FeedFetcher> fetchers = new ArrayList<com.sun.syndication.fetcher.FeedFetcher>();
		for (int i = 0; i < num; i++) {
			fetchers.add(new HttpURLFeedFetcher(cache));
		}
		return fetchers;
	}


}
