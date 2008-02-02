require "classification/nbayes"

class Recommendations
  
  def initialize datasource
    @ds = datasource
  end

  def recommendations_for user
    rec = []
    classifier = Verditz::NaiveBayesClassifier.new
    classifier.set_threshold(:good, 4)

    train(classifier, user.upvotes, :good)
    train(classifier, user.downvotes, :bad)

    @ds.articles(user) do |article|
      result = classifier.classify("#{article.title}\n#{article.body}")
      if result[:guess] == :good
        rec << {:id => article.id, :score => result[:score]}
      end
    end
    rec
  end

  def update_recommendations 
    @ds.users.each do |user|
      articles = recommendations_for(user)
      @ds.set_recommendations(user, articles)
      yield user, articles
    end
  end

  def train classifier, documents, cat
    documents.each do |doc|
      10.times{ classifier.train doc.title, cat }
      classifier.train(doc.body, cat)
    end 
  end

end

require "mysql"


class VerditzDs

  User = Struct.new(:id, :upvotes, :downvotes)
  Article = Struct.new(:id,:title, :body)


  def initialize
    @db = Mysql.new("localhost", "root", "", "verditz_snapshot")
  end

  def set_recommendations user, articles
    recs = articles.sort_by{|a|a[:score]}.reverse
    @db.query("delete from recommendations where user_id = #{user.id}")
    recs[0..50].each do |article|
      @db.query("insert into recommendations (user_id, article_id, score) values (#{user.id}, #{article[:id]}, #{article[:score]})")
      @db.commit
    end
  end
  
  def users
    res = @db.query("select id from users where id = 2 order by id ASC")
    users = []
    res.each do |row| 
      user = User.new(row[0])
      user.upvotes = collect_upvotes_for user
      user.downvotes = collect_downvotes_for user
      users << user
    end
    users
  end

  def collect_upvotes_for userid
    collect_votes_for userid, 1
  end

  def collect_downvotes_for userid
    collect_votes_for userid, -1
  end

  def collect_votes_for user, value
    articles = []
    res = @db.query("select a.id, a.title, a.text from articles a, votes v where v.user_id = #{user.id} and v.article_id = a.id and v.value = #{1}  order by publish_time DESC limit 5000")    
    for row in res
      articles << Article.new(row[0],row[1],row[2])
    end
    articles
  end

  def articles user
    res = @db.query("select id, title, text from articles WHERE id not in (select article_id from votes WHERE user_id = #{user.id} ) order by publish_time DESC")
    res.each do |row|
      yield Article.new(row[0], row[1], row[2])
    end
  end

end

# ds = VerditzDs.new
# user = ds.users.first

# ds.articles(user) do |article|

#   p article.id

# end



rec = Recommendations.new(VerditzDs.new)
rec.update_recommendations do |user, articles|
  puts "USER: #{user.id}"
  puts
  articles.sort_by{|a|a[:score]}.reverse.each do |article|
    puts "#{article[:id]}, #{article[:score]}"
  end
end


