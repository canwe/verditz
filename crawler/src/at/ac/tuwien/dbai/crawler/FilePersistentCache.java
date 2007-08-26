package at.ac.tuwien.dbai.crawler;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.URL;

import com.sun.syndication.fetcher.impl.FeedFetcherCache;
import com.sun.syndication.fetcher.impl.HashMapFeedInfoCache;
import com.sun.syndication.fetcher.impl.SyndFeedInfo;

public final class FilePersistentCache implements FeedFetcherCache {

	public static final String CACHE_FILE = "cache";
	
	private final FeedFetcherCache internalCache;

	public FilePersistentCache() {
		internalCache = this.loadCache();
	}

	public SyndFeedInfo getFeedInfo(URL url) {
		return this.internalCache.getFeedInfo(url);
	}

	public void setFeedInfo(URL url, SyndFeedInfo info) {
		this.internalCache.setFeedInfo(url, info);
	}

	public FeedFetcherCache loadCache() {
		try {
			if (new File(CACHE_FILE).exists()) {
				final FileInputStream f_in = new FileInputStream(CACHE_FILE);
				final ObjectInputStream obj_in = new ObjectInputStream(f_in);
				return (FeedFetcherCache) obj_in.readObject();
			} else {
				return HashMapFeedInfoCache.getInstance();
			}
		} catch (Exception e) {
			throw new RuntimeException(e.getMessage());
		}
	}

	public void saveCache() {
		try {
			final FileOutputStream f_out = new FileOutputStream(CACHE_FILE);
			final ObjectOutputStream obj_out = new ObjectOutputStream(f_out);
			obj_out.writeObject(this.internalCache);
		} catch (Exception e) {
			throw new RuntimeException(e.getMessage());
		}

	}

}
