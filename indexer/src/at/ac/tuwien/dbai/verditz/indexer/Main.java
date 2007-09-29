package at.ac.tuwien.dbai.verditz.indexer;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Collection;
import java.util.InvalidPropertiesFormatException;

import org.apache.log4j.Logger;

import at.ac.tuwien.dbai.verditz.indexer.crawler.ArticleCrawler;
import at.ac.tuwien.dbai.verditz.indexer.crawler.Crawler;
import at.ac.tuwien.dbai.verditz.indexer.db.Article;
import at.ac.tuwien.dbai.verditz.indexer.db.DatabaseIndexer;

public class Main {
	private DatabaseIndexer indexer;
	private Crawler crawler;
	private final static Logger log = Logger.getLogger(Main.class);
	
	public Main() {
		try {
			this.indexer = new DatabaseIndexer();
		} catch (InvalidPropertiesFormatException e) {
			e.printStackTrace();
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		this.crawler = new ArticleCrawler(this.indexer.getFeeds());
	}
	
	public void start() {
		Collection<Article> articles = this.crawler.fetchArticles();
		indexer.addArticles(articles);
	}

	public static void main(String[] args) {
		log.info("Starting indexer...");
		new Main().start();
	}

}
