class AddRecommendationIndices < ActiveRecord::Migration
  def self.up
    add_index :recommendations, :user_id
    add_index :recommendations, :article_id
  end

  def self.down
  end
end
