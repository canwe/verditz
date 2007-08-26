package at.ac.tuwien.dbai.verditz.crawler.test;

import java.io.File;

import junit.framework.TestCase;
import at.ac.tuwien.dbai.crawler.FeedCrawler;
import at.ac.tuwien.dbai.crawler.FlatFileSource;
import at.ac.tuwien.dbai.verditz.crawler.DocumentCrawler;

public class DocumentCrawlerTest extends TestCase{

	
	public void test(){
		FeedCrawler crawler = new FeedCrawler();

		crawler.addFeedSource(new FlatFileSource(new File("feeds.txt")));

		crawler.addFetcherEventListener(new DocumentCrawler());

		crawler.fetch();
	}
	
}
