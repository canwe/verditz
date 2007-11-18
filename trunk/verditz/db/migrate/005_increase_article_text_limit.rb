class IncreaseArticleTextLimit < ActiveRecord::Migration
  def self.up
    change_column :articles, :text, :text, :limit => (2**24)
  end

  def self.down
    change_column :articles, :text, :text, :limit => (2**16 - 1)
  end
end
