package at.ac.tuwien.dbai.verditz.indexer.db;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URL;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.InvalidPropertiesFormatException;
import java.util.Map;
import java.util.Properties;

import javax.sql.DataSource;

import org.apache.commons.dbcp.BasicDataSource;


public class DatabaseIndexer {
	private final DataSource dataSource;

	public DatabaseIndexer() throws InvalidPropertiesFormatException, FileNotFoundException, IOException {
		Map<String, String> properties = loadPropertiesFromFile();
		this.dataSource = this.setupDataSource(properties.get("jdbcUrl"), properties.get("user"), properties.get("password"), properties.get("jdbcDriver"));
	}

	public DatabaseIndexer(Map<String, String> properties) {
		this.dataSource = this.setupDataSource(properties.get("jdbcUrl"), properties.get("user"), properties.get("password"), properties.get("jdbcDriver"));
	}
	
	public DatabaseIndexer(String jdbcUrl, String user, String password, String jdbcDriver) {
		this.dataSource= this.setupDataSource(jdbcUrl, user, password, jdbcDriver);
	}
	
	public void addFeed(URL url) {
		
	}
	
	public void addArticle(Article article) throws SQLException {
		Connection conn = this.dataSource.getConnection();
		conn.prepareStatement("");
	}
	
	public void addArticles(Collection<Article> articles) {
		
	}
	
	public Collection<URL> getFeeds() {
		return new ArrayList<URL>();
	}
	
	private DataSource setupDataSource(String jdbcUrl, String user, String password, String jdbcDriver) {
		BasicDataSource dataSource = new BasicDataSource();
		dataSource.setUrl(jdbcUrl);
		dataSource.setUsername(user);
		dataSource.setPassword(password);
		dataSource.setDriverClassName(jdbcDriver);
		return dataSource;
	}

	private Map<String, String> loadPropertiesFromFile()
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
	
	private File searchDatabaseSettings() {
		File dbSettingsFile;
		if (System.getProperties().containsKey("databaseSettings")) {
			dbSettingsFile = new File(System.getProperty("databaseSettings"));
		} else {
			throw new IllegalArgumentException("No properties file found");
		}
		return dbSettingsFile;
	}
}
