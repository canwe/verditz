require 'hpricot'
require 'htmlentities'
require 'open-uri'
require 'iconv'

class Article < ActiveRecord::Base
  has_many :recommendations
  has_many :users, :through => :recommendations
  belongs_to :source

  def self.index(url)
    article = Article.find(:first, :conditions => ["url = ?", url])
    if article.nil?
      resource = open(url)
      charset = resource.charset()
      doc = Hpricot(resource)
      title = doc.at("title").inner_text
      body = doc.at("body").inner_html
      body = body.gsub(/<script.*?<\/script>/m, "")
      body = body.gsub(/<style.*?<\/style>/m, "")
      body = body.gsub(/<.*?>/m, "")
      coder = HTMLEntities.new
      body = coder.decode(body)
      article = Article.new
      article.title = Iconv.new("UTF-8", charset).iconv(title)
      article.text = Iconv.new("UTF-8", charset).iconv(body)
      article.publish_time = Time.now
      article.url = url
      article.save
    end
    return article.id
  end
end