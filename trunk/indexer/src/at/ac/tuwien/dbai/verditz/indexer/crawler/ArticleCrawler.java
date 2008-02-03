package at.ac.tuwien.dbai.verditz.indexer.crawler;

import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.apache.log4j.Logger;
import org.htmlparser.util.ParserException;

import at.ac.tuwien.dbai.verditz.crawler.FeedCrawler;
import at.ac.tuwien.dbai.verditz.html2text.Html2Text;
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
					log.info("retrieved feed: " + feed.getTitle());
					for (SyndEntry entry : (List<SyndEntry>) feed.getEntries()) {
						try {
							Article article = new Article();
							article.setTitle(entry.getTitle());
							String text = Html2Text.html2Text(this
									.getFeedBody(entry));
							article.setText(text);
							article.setPublishTime(entry.getPublishedDate());
							URL url = RedirectResolver.resolve(new URL(entry
									.getLink()));
							article.setUrl(url);
							Source source = new Source();
							source.setUrl(new URL(event.getUrlString()));
							article.setSource(source);
							articles.add(article);
						} catch (IOException e) {
							log.error("could not index article", e);
						} catch (ParserException e) {
							log.error("could not index article", e);
						}
					}
				}
			}

			private String getFeedBody(SyndEntry entry) {
				final StringBuilder sb = new StringBuilder();
				List<SyndContent> contents = entry.getContents();
				if (entry.getDescription() != null) {
					contents.add(entry.getDescription());
				}
				for (SyndContent content : contents) {
					String type = "text/plain";
					if (content.getType() != null) {
						type = content.getType();
					}
					
					if (this.isSupportedContentType(type.toLowerCase())) {
						sb.append(content.getValue());
					}
				}
				return sb.toString();
			}

			private boolean isSupportedContentType(final String type) {
				Collection<String> supportedContentTypes = Arrays
						.asList(new String[] { "text/plain", "text/html",
								"text", "html", "application/xhtml+xml" });

				return supportedContentTypes.contains(type);
			}

		};
		FeedCrawler crawler = new FeedCrawler(handler, this.feeds);
		log.info("fetching articles...");
		crawler.fetch();
		log.info("done with fetching articles");
		return articles;
	}

}
