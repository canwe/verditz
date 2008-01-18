require 'digest/sha1'

class User < ActiveRecord::Base
  validates_presence_of :name
  validates_uniqueness_of :name

  attr_accessor :password_confirmation
  validates_confirmation_of :password

  has_many :recommendations, :dependent => :destroy
  has_many :votes, :dependent => :destroy
  has_many :articles, :through => :recommendations

  def validate
    errors.add_to_base("Missing password") if hashed_password.blank?
  end

  def self.authenticate(name, password)
    user = self.find_by_name(name)
    if user
      expected_password = encrypted_password(password, user.salt)
      if user.hashed_password != expected_password
        user = nil
      end
    end
    user
  end

  def password
    @password
  end

  def password=(pwd)
    @password = pwd
    return if pwd.blank?
    create_new_salt
    self.hashed_password = User.encrypted_password(self.password, self.salt)
  end

  def vote(article, value)
    vote = Vote.find(:first, :conditions => ["user_id = ? and article_id = ?", self.id, article.id])
    if value == 0
      vote.destroy unless vote.nil?
      return
    elsif vote.nil?
      vote = Vote.new
    end
    vote.value = value
    vote.user = self
    vote.article = article
    vote.createtime = Time.now
    vote.save
  end

  private
  def self.encrypted_password(password, salt)
    string_to_hash = password + "signanz" + salt # 'signanz' makes it harder to guess
    Digest::SHA1.hexdigest(string_to_hash)
  end

  def create_new_salt
    self.salt = self.object_id.to_s + rand.to_s
  end
end
