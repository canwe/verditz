class ChangeScoreToFloat < ActiveRecord::Migration
  def self.up
    change_column :recommendations, :score, :float
  end

  def self.down
    change_column :recommendations, :score, :decimal, :precision => 30, :scale => 29
  end
end
