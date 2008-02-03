require "classification/nbayes"

class Recommendations
  
  def initialize datasource, threshold, titleboost, max_recommendations
    @ds = datasource
    @threshold = threshold
    @titleboost = titleboost
    @max_recommendations = max_recommendations
  end

  def recommendations_for user
    rec = []
    classifier = Verditz::NaiveBayesClassifier.new
    classifier.set_threshold(:good, @threshold)
    classifier.set_threshold(:bad, 1)

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
      if @ds.active_user? user
        articles = recommendations_for(user)
        @ds.set_recommendations(user, articles, @max_recommendations)
        yield user, articles
      end
    end
  end

  def train classifier, documents, cat
    documents.each do |doc|
      @titleboost.times{ classifier.train(doc.title, cat) }
      classifier.train(doc.body, cat)
    end 
  end

end

require "mysql"
require "date"

class VerditzDs

  User = Struct.new(:id, :name, :upvotes, :downvotes)
  Article = Struct.new(:id,:title, :body)


  def initialize host, username, password, database, user_active_days
    @db = Mysql.new(host, username, password, database)
    @user_active_days = user_active_days
  end

  def set_recommendations user, articles, limit
    recs = articles.sort_by{|a|a[:score]}.reverse
    @db.query("delete from recommendations where user_id = #{user.id}")
    recs[0..limit].each do |article|
      @db.query("insert into recommendations (user_id, article_id, score) values (#{user.id}, #{article[:id]}, #{article[:score]})")
      @db.commit
    end
  end
  
  def users
    res = @db.query("select id, name from users order by name ASC")
    users = []
    res.each do |row| 
      user = User.new(row[0],row[1])
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

  def active_user? user
    res = @db.query("select max(createtime) from votes where user_id = #{user.id}")
    date_str =  res.fetch_row[0]
    time = DateTime.parse(date_str)
    now = DateTime.now
    flag = (time + @user_active_days) > now
    puts "User is active: #{flag}"
    flag
  end

  def collect_votes_for user, value
    articles = []
    res = @db.query("select a.id, a.title, a.text from articles a, votes v where v.user_id = #{user.id} and v.article_id = a.id and v.value = #{value}  order by publish_time DESC limit 5000")    
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
require "yaml"
opt = YAML::load(File.new("/home/ferrari/.verditz/config.yml"))["test"]
rec = Recommendations.new(VerditzDs.new(opt["host"], opt["username"], 
                                        opt["password"], opt["database"], opt["user_active_days"].to_i), opt["threshold"].to_i, opt["titleboost"].to_i, opt["max_recommendations"].to_i)
rec.update_recommendations do |user, articles|
  puts "updated recommendations for #{user.name}"
end


