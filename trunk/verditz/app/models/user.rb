class User < ActiveRecord::Base
  has_many :recommendations
  has_many :articles, :through => :recommendations
end
