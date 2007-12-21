class CreateRecommendationTable < ActiveRecord::Migration
  def self.up
    create_table :recommendations do |t|
      t.column :user_id, :integer
      t.column :article_id, :integer
      t.column :score, :decimal, :precision => 10, :scale => 9
    end
  end

  def self.down
    drop_table :recommendations
  end
end
