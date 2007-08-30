package at.ac.tuwien.dbai.verditz.crawler.databaseindexer;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.Map;

import javax.sql.DataSource;

import org.apache.commons.dbcp.BasicDataSource;
import org.apache.log4j.Logger;

/**
 * Current database is very simple: one table with title, body, sourUrl. That's
 * all I need by now do experiment with WEKA.
 */
public final class DocumentDatabase {

	final Logger log = Logger.getLogger(DocumentDatabase.class);
	private final DataSource dataSource;

	public DocumentDatabase(final Map<String, String> dbsettings) {
		this(dbsettings.get("driver"), dbsettings.get("jdbcurl"), dbsettings
				.get("username"), dbsettings.get("password"));
	}

	public DocumentDatabase(String driver, String jdbcUrl, String username,
			String password) {
		this.dataSource = DocumentDatabase.setupDataSource(driver, jdbcUrl,
				username, password);
	}

	private static DataSource setupDataSource(String driver, String jdbcUrl,
			String username, String password) {
		BasicDataSource ds = new BasicDataSource();
		ds.setDriverClassName(driver);
		ds.setUrl(jdbcUrl);
		ds.setUsername(username);
		ds.setPassword(password);
		return ds;
	}

	public void addDocument(String title, String body, String source) {
		Connection conn = null;

		if (title == null || body == null || source == null) {
			log.info("one parameter was null:" + title);
			return;
		}

		try {
			conn = this.dataSource.getConnection();
			conn.setAutoCommit(false);

			PreparedStatement queryStmt = conn
					.prepareStatement("select 'X' from documents where source = ?");
			queryStmt.setString(1, source);
			if (queryStmt.executeQuery().next()) {
				log.info("already in database: " + title);
				return;
			}

			PreparedStatement stmt = conn
					.prepareStatement("insert into documents (title,body,source) values (?,?,?)");
			stmt.setString(1, title);
			stmt.setString(2, body);
			stmt.setString(3, source);
			stmt.execute();
			conn.commit();
			log.info("added " + title + " to database");
		} catch (SQLException e) {
			log.error(e);
		} finally {
			try {
				conn.close();
			} catch (SQLException e) {
				e.printStackTrace();
			}
		}
	}
}
