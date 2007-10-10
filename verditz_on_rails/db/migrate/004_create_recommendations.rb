class CreateRecommendations < ActiveRecord::Migration
  def self.up
    create_table :recommendations do |t|
    end
  end

  def self.down
    drop_table :recommendations
  end
end
