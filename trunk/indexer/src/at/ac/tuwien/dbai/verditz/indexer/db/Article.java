package at.ac.tuwien.dbai.verditz.indexer.db;

import java.net.URL;
import java.util.Date;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Article {
	private Integer id;

	private String title;

	private String text;

	private URL url;

	private Date publishTime;

	private Source source;
	
	private static Pattern tagPattern = Pattern.compile("<[^>]*>");

	public void setId(Integer id) {
		this.id = id;
	}

	public void setPublishTime(Date publishTime) {
		this.publishTime = publishTime;
	}
	
	public void setSource(Source source) {
		this.source = source;
	}

	public void setText(String text) {
		this.text = text;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public void setUrl(URL url) {
		this.url = url;
	}

	public Integer getId() {
		return id;
	}

	public Date getPublishTime() {
		return publishTime;
	}

	public Source getSource() {
		return source;
	}
	
	public String getPlainText() {
		Matcher matcher = Article.tagPattern.matcher(this.getText());
		return matcher.replaceAll("");
	}

	public String getText() {
		return text;
	}

	public String getTitle() {
		return title;
	}

	public URL getUrl() {
		return url;
	}
}
