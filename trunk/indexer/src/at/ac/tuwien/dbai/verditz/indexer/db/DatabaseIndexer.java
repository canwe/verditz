package at.ac.tuwien.dbai.verditz.indexer.db;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.InvalidPropertiesFormatException;
import java.util.Map;
import java.util.Properties;

import javax.sql.DataSource;

import org.apache.commons.dbcp.BasicDataSource;
import org.apache.log4j.Logger;

public class DatabaseIndexer {
	private final DataSource dataSource;

	private final static Logger log = Logger.getLogger(DatabaseIndexer.class);

	public DatabaseIndexer() throws InvalidPropertiesFormatException,
			FileNotFoundException, IOException {
		Map<String, String> properties = loadPropertiesFromFile();
		this.dataSource = this.setupDataSource(properties.get("jdbcUrl"),
				properties.get("username"), properties.get("password"),
				properties.get("jdbcDriver"));
	}

	public DatabaseIndexer(Map<String, String> properties) {
		this.dataSource = this.setupDataSource(properties.get("jdbcUrl"),
				properties.get("username"), properties.get("password"),
				properties.get("jdbcDriver"));
	}

	public DatabaseIndexer(String jdbcUrl, String user, String password,
			String jdbcDriver) {
		this.dataSource = this.setupDataSource(jdbcUrl, user, password,
				jdbcDriver);
	}

	public void addFeed(URL url) throws SQLException {
		Connection conn = null;
		try {
			conn = this.dataSource.getConnection();
			this.addFeed(url, conn);
		} catch (SQLException e) {
			throw e;
		} finally {
			conn.close();
		}
	}

	public void addFeed(URL url, Connection conn) throws SQLException {
		PreparedStatement stmt = conn
				.prepareStatement("insert into feed(url) values(?)");
		stmt.setString(1, url.toString());
		stmt.executeUpdate();
	}

	public void addArticle(Article article) throws SQLException {
		Connection conn = null;
		try {
			conn = this.dataSource.getConnection();
			conn.setAutoCommit(false);
			this.addArticle(article);
			conn.commit();
		} catch (SQLException e) {
			throw e;
		} finally {
			conn.close();
		}
	}

	public void addArticle(Article article, Connection conn)
			throws SQLException {
		PreparedStatement stmt = conn.prepareStatement("insert into article "
				+ "(text, title, url, publish_time, f_source) values(?, ?, ?, ?, ?)");
		stmt.setString(1, article.getPlainText());
		stmt.setString(2, article.getTitle());
		stmt.setString(3, article.getUrl().toString());
		stmt.setDate(4, DatabaseHelpers.toSqlDate(article.getPublishTime()));
		stmt.setInt(5, article.getSource().getId());
		stmt.execute();
	}

	public void addArticles(Collection<Article> articles) throws SQLException {
		Connection conn = null;
		try {
			conn = this.dataSource.getConnection();
			conn.setAutoCommit(false);
			for (Article article : articles) {
				this.addArticle(article, conn);
			}
			conn.commit();
		} catch (SQLException e) {
			throw e;
		} finally {
			conn.close();
		}
	}

	public Collection<URL> getFeeds() throws SQLException {
		Collection<URL> feeds = new ArrayList<URL>();
		Connection conn = null;
		try {
			conn = this.dataSource.getConnection();
			PreparedStatement stmt = conn
					.prepareStatement("select url from feed");
			stmt.execute();
			ResultSet rs = stmt.getResultSet();
			while (rs.next()) {
				String stringUrl = rs.getString("url");
				try {
					URL url = new URL(stringUrl);
					feeds.add(url);
				} catch (MalformedURLException e) {
					log.error("malformed url in database table feed: "
							+ stringUrl);
				}
			}
		} catch (SQLException e) {
			throw e;
		} finally {
			conn.close();
		}
		return feeds;
	}

	private DataSource setupDataSource(String jdbcUrl, String user,
			String password, String jdbcDriver) {
		BasicDataSource dataSource = new BasicDataSource();
		dataSource.setUrl(jdbcUrl);
		dataSource.setUsername(user);
		dataSource.setPassword(password);
		dataSource.setDriverClassName(jdbcDriver);
		return dataSource;
	}

	@SuppressWarnings("unchecked")
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
