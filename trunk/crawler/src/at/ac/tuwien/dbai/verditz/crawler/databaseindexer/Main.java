package at.ac.tuwien.dbai.verditz.crawler.databaseindexer;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Arrays;
import java.util.Collection;
import java.util.InvalidPropertiesFormatException;
import java.util.LinkedList;
import java.util.Map;
import java.util.Properties;
import java.util.Scanner;

import at.ac.tuwien.dbai.verditz.crawler.DefaultFetchStrategy;
import at.ac.tuwien.dbai.verditz.crawler.FeedCrawler;
import at.ac.tuwien.dbai.verditz.crawler.FetchStrategy;
import at.ac.tuwien.dbai.verditz.crawler.FlatFileSource;

public class Main {

	public static void main(String[] args)
			throws InvalidPropertiesFormatException, FileNotFoundException,
			IOException {

		Iterable<URL> feedSource = searchFeedSource();
		DatabaseIndexer dbi = new DatabaseIndexer(searchDatabaseProperties());
		FetchStrategy fetchStrategy = new DefaultFetchStrategy(10);
		fetchStrategy.addObserver(dbi);

		FeedCrawler crawler = new FeedCrawler(fetchStrategy);
		crawler.addFeedSource(feedSource);
		crawler.fetch();

	}

	private static Iterable<URL> searchFeedSource() {
		Iterable<URL> source;
		if (System.getProperties().containsKey("feedsFile")) {
			source = new FlatFileSource(new File(System
					.getProperty("feedsFile")));
		} else {
			Collection<URL> feeds = new LinkedList<URL>();
			Scanner scanner = new Scanner(System.in);
			while (scanner.hasNextLine()) {
				try {
					String line = scanner.nextLine();
					if (line != null && line.trim().length() > 5) {
						feeds.add(new URL(line));
					}
				} catch (MalformedURLException e) {
					e.printStackTrace();
				}
			}
			source = feeds;
		}
		return source;
	}

	@SuppressWarnings("unchecked")
	private static Map<String, String> searchDatabaseProperties()
			throws InvalidPropertiesFormatException, FileNotFoundException,
			IOException {
		Properties databaseProperties = new Properties();

		File dbSettingsFile = searchDatabaseSettings();
		if (dbSettingsFile.getName().endsWith(".xml")) {
			databaseProperties.loadFromXML(new FileInputStream(dbSettingsFile));
		} else {
			databaseProperties.load(new FileInputStream(dbSettingsFile));
		}

		return (Map<String, String>) (Map) databaseProperties;
	}

	private static File searchDatabaseSettings() {
		File dbSettingsFile;
		if (System.getProperties().containsKey("databaseSettings")) {
			dbSettingsFile = new File(System.getProperty("databaseSettings"));
		} else {
			throw new IllegalArgumentException("No properties file found");
		}
		return dbSettingsFile;
	}

}
