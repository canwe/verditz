package at.ac.tuwien.dbai.verditz.indexer.crawler;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.apache.log4j.Logger;

import at.ac.tuwien.dbai.verditz.crawler.FeedCrawler;
import at.ac.tuwien.dbai.verditz.indexer.db.Article;
import at.ac.tuwien.dbai.verditz.indexer.db.Source;

import com.sun.syndication.feed.synd.SyndContent;
import com.sun.syndication.feed.synd.SyndEntry;
import com.sun.syndication.feed.synd.SyndFeed;
import com.sun.syndication.fetcher.FetcherEvent;
import com.sun.syndication.fetcher.FetcherListener;

public class ArticleCrawler implements Crawler<URL> {
	private Collection<URL> feeds = new ArrayList<URL>();
	
	private final static Logger log = Logger.getLogger(ArticleCrawler.class);
	
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

			@SuppressWarnings("unchecked")
			public void fetcherEvent(FetcherEvent event) {
				if (event.getEventType() == FetcherEvent.EVENT_TYPE_FEED_RETRIEVED) {
					SyndFeed feed = event.getFeed();
					log.info("retrieved feed: " + feed.getLink());
					for (SyndEntry entry : (List<SyndEntry>) feed.getEntries()) {
						try {
						Article article = new Article();
						article.setTitle(entry.getTitle());
						article.setText(this.getFeedBody(entry.getContents()));
						article.setPublishTime(entry.getPublishedDate());
						article.setUrl(new URL(entry.getLink()));
						Source source = new Source();
						source.setUrl(new URL(event.getUrlString()));
						article.setSource(source);
						articles.add(article);
						} catch(MalformedURLException e) {
							log.error("could not index article because of malformed URL: " + entry.getLink());
						}
					}
				}
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
			
		};
		FeedCrawler crawler = new FeedCrawler(handler);
		crawler.addFeedSource(this.feeds);
		log.info("fetching articles...");
		crawler.fetch();
		log.info("done with fetching articles");
		return articles;
	}
	
	

}
