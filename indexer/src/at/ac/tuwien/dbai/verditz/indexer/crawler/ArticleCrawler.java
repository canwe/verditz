package at.ac.tuwien.dbai.verditz.indexer.crawler;

import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;

import at.ac.tuwien.dbai.verditz.crawler.FeedCrawler;
import at.ac.tuwien.dbai.verditz.indexer.db.Article;

import com.sun.syndication.fetcher.FetcherEvent;
import com.sun.syndication.fetcher.FetcherListener;

public class ArticleCrawler implements Crawler<URL> {
	private Collection<URL> feeds = new ArrayList<URL>();
	
	public ArticleCrawler() {
	}
	
	public ArticleCrawler(Collection<URL> feeds) {
		this.setSources(feeds);
	}
	
	public void setSources(Collection<URL> sources) {
		this.feeds = sources;
	}

	public Collection<Article> fetchArticles() {
		final Collection<Article> articles = new ArrayList<Article>();
		final FetcherListener handler = new FetcherListener() {

			public void fetcherEvent(FetcherEvent event) {
				if (event.getEventType() == FetcherEvent.EVENT_TYPE_FEED_RETRIEVED) {
					event.getFeed().getEntries().size();
					System.out.println(event.getFeed().getTitle());
					
					Article article = new Article();
					articles.add(article);
				}
			}
			
		};
		FeedCrawler crawler = new FeedCrawler(handler);
		crawler.addFeedSource(this.feeds);
		crawler.fetch();
		return articles;
	}

}
