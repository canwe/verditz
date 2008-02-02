class ChangePrecisionOfScore < ActiveRecord::Migration
  def self.up
    change_column :recommendations, :score, :decimal, :precision => 30, :scale => 29
  end

  def self.down
    change_column :recommendations, :score, :decimal, :precision => 10, :scale => 9
  end
end
