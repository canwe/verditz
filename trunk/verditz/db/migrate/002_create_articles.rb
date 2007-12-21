class CreateArticles < ActiveRecord::Migration
  def self.up
    create_table :articles do |t|
      t.column :title, :string, :limit => 1024
      t.column :text, :text
      t.column :publish_time, :timestamp
      t.column :url, :string, :limit => 1024, :null => false
      t.column :source_id, :integer, :null => false
    end

    add_index :articles, :source_id
  end

  def self.down
    drop_table :articles
  end
end
