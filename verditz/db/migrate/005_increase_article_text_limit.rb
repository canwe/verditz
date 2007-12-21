class IncreaseArticleTextLimit < ActiveRecord::Migration
  def self.up
    change_column :articles, :text, :string, :limit => (2**23)
  end

  def self.down
    change_column :articles, :text, :text
  end
end
