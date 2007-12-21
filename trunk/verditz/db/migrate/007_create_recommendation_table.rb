class CreateRecommendationTable < ActiveRecord::Migration
  def self.up
      add_column :recommendations, :user_id, :integer
      add_column :recommendations, :article_id, :integer
      add_column :recommendations, :score, :decimal, :precision => 10, :scale => 9
  end

  def self.down
  end
end
