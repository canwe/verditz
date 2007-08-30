package at.ac.tuwien.dbai.verditz.crawler.test;

import java.io.File;

import junit.framework.TestCase;
import at.ac.tuwien.dbai.verditz.crawler.DocumentCrawler;
import at.ac.tuwien.dbai.verditz.crawler.FeedCrawler;
import at.ac.tuwien.dbai.verditz.crawler.FlatFileSource;

public class DocumentCrawlerTest extends TestCase{

	
	public void test(){
		FeedCrawler crawler = new FeedCrawler(new DocumentCrawler());

		crawler.addFeedSource(new FlatFileSource(new File("resources/feeds.txt")));


		crawler.fetch();
	}
	
}
