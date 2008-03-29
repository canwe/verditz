class UniqueArticleUrl < ActiveRecord::Migration
  def self.up
    articles = Article.find_by_sql("select url from articles group by url having count(*) > 1")
    articles.each do |article|
      Article.delete(:conditions => ["url = ?", article.url])
    end
    add_index :articles, :url, :unique => true
  end

  def self.down
    remove_index :users, :url
  end
end
