class Article < ActiveRecord::Base
  has_many :recommendations
  has_many :users, :through => :recommendations
  belongs_to :source
end
