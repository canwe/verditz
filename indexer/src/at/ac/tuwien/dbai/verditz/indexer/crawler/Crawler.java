package at.ac.tuwien.dbai.verditz.indexer.crawler;

import java.util.Collection;

import at.ac.tuwien.dbai.verditz.indexer.db.Article;

public interface Crawler<T> {
	public void setSources(Collection<T> sources);
	public Collection<Article> fetchArticles();
}
