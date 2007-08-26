package at.ac.tuwien.dbai.crawler;

import java.net.URL;
import java.util.Collection;

import com.sun.syndication.fetcher.FetcherListener;

public interface FetchStrategy {

	void addObserver(FetcherListener observer);

	void removeObserver(FetcherListener observer);

	void execute(Collection<Iterable<URL>> sources);

}
