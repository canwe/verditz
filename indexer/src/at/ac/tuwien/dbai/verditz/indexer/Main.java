package at.ac.tuwien.dbai.verditz.indexer;

import java.sql.SQLException;
import java.util.Collection;

import org.apache.log4j.Logger;

import at.ac.tuwien.dbai.verditz.indexer.crawler.ArticleCrawler;
import at.ac.tuwien.dbai.verditz.indexer.crawler.Crawler;
import at.ac.tuwien.dbai.verditz.indexer.db.Article;
import at.ac.tuwien.dbai.verditz.indexer.db.DatabaseIndexer;

public class Main {
	private DatabaseIndexer indexer;

	private Crawler crawler;

	private final static Logger log = Logger.getLogger(Main.class);

	public Main() throws IndexerException {
		try {
			this.indexer = new DatabaseIndexer();
			this.crawler = new ArticleCrawler(this.indexer.getFeeds());
		} catch (Exception e) {
			log.fatal("fatal error occured during startup:", e);
			throw new IndexerException(e);
		}
	}

	@SuppressWarnings("unchecked")
	public void start() throws IndexerException {
		Collection<Article> articles = this.crawler.fetchArticles();
		try {
			indexer.addArticles(articles);
		} catch (SQLException e) {
			log.error("error occured while inserting articles into database:");
			throw new IndexerException(e);
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
