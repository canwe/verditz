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
import java.util.Date;
import java.util.InvalidPropertiesFormatException;
import java.util.Map;
import java.util.Properties;

import javax.sql.DataSource;

import org.apache.commons.dbcp.BasicDataSource;
import org.apache.log4j.Logger;

import at.ac.tuwien.dbai.verditz.indexer.IndexerException;

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

	public int addArticle(Article article) throws SQLException {
		Connection conn = null;
		try {
			conn = this.dataSource.getConnection();
			conn.setAutoCommit(false);
			int added = this.addArticle(article);
			conn.commit();
			return added;
		} catch (SQLException e) {
			throw e;
		} finally {
			conn.close();
		}
	}

	public int addArticle(Article article, Connection conn)
			throws SQLException, IndexerException {
		if (this.articleExists(article.getUrl(), conn))
			return 0;
		
		log.info("indexing article: " + article.getUrl());
		PreparedStatement stmt = conn
				.prepareStatement("insert into articles "
						+ "(text, title, url, publish_time, source_id) values(?, ?, ?, ?, ?)");
		stmt.setString(1, article.getPlainText());
		stmt.setString(2, article.getTitle());
		stmt.setString(3, article.getUrl().toString());
		if (article.getPublishTime() != null)
			stmt.setTimestamp(4, new java.sql.Timestamp(article.getPublishTime()
					.getTime()));
		else
			stmt.setTimestamp(4, new java.sql.Timestamp(new Date().getTime()));
		if (article.getSource().getId() != null)
			stmt.setInt(5, article.getSource().getId());
		else {
			stmt.setInt(5, this.getFeedIdFromUrl(article.getSource().getUrl(),
					conn));
		}
		stmt.execute();
		log.info("indexed article: " + article.getUrl());
		return 1;
	}

	public boolean articleExists(URL url, Connection conn) throws SQLException {
		PreparedStatement stmt = conn
				.prepareStatement("select 'X' from articles where url = ?");
		stmt.setString(1, url.toString());
		ResultSet rs = stmt.executeQuery();
		return rs.next();
	}

	public int addArticles(Collection<Article> articles) throws SQLException,
			IndexerException {
		Connection conn = null;
		int added = 0;
		try {
			conn = this.dataSource.getConnection();
			conn.setAutoCommit(false);
			log.info("indexing articles");
			for (Article article : articles) {
				added += this.addArticle(article, conn);
			}
			conn.commit();
			if (added > 0)
				log.info("Indexer successfully indexed " + added + " article(s)");
			else
				log.info("No new articles to index");
			return added;
		} catch (SQLException e) {
			throw e;
		} finally {
			conn.close();
		}
	}

	private int getFeedIdFromUrl(URL url, Connection conn) throws SQLException,
			IndexerException {
		PreparedStatement stmt = conn
				.prepareStatement("select id from sources where url = ?");
		stmt.setString(1, url.toString());
		ResultSet rs = stmt.executeQuery();
		if (rs.next())
			return rs.getInt("id");
		throw new IndexerException("no feed exists with url: " + url.toString());
	}

	public Collection<URL> getFeeds() throws SQLException {
		Collection<URL> feeds = new ArrayList<URL>();
		Connection conn = null;
		try {
			conn = this.dataSource.getConnection();
			PreparedStatement stmt = conn
					.prepareStatement("select url from sources");
			ResultSet rs = stmt.executeQuery();
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
