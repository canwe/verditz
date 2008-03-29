package at.ac.tuwien.dbai.verditz.indexer;

import java.net.URL;
import java.sql.SQLException;
import java.util.Collection;

import org.apache.log4j.Logger;

import at.ac.tuwien.dbai.verditz.indexer.crawler.ArticleCrawler;
import at.ac.tuwien.dbai.verditz.indexer.crawler.Crawler;
import at.ac.tuwien.dbai.verditz.indexer.db.Article;
import at.ac.tuwien.dbai.verditz.indexer.db.DatabaseIndexer;

public class Main {
	private DatabaseIndexer indexer;

	private final static Logger log = Logger.getLogger(Main.class);

	public Main() throws IndexerException {
		try {
			this.indexer = new DatabaseIndexer();
		} catch (Exception e) {
			log.fatal("fatal error occured during startup:", e);
			throw new IndexerException(e);
		}
	}

	@SuppressWarnings("unchecked")
	public void start() throws IndexerException {
		Collection<URL> feeds;
		try {
			feeds = this.indexer.getFeeds();
		} catch (SQLException e1) {
			throw new IndexerException(e1);
		}
		for (URL feed : feeds) {
			Crawler crawler = new ArticleCrawler(feed);
			Collection<Article> articles = crawler.fetchArticles();
			try {
				log.info("indexing articles for " + feed.toString());
				this.indexer.addArticles(articles);
			} catch (SQLException e) {
				log
						.error("error occured while inserting articles into database:");
			}
		}
	}

	public static void main(String[] args) {
		log.info("Starting indexer...");
		try {
			new Main().start();
			log.info("Indexer done");
		} catch (IndexerException e) {
			log.error("", e);
		}
	}

}
