class CreateArticles < ActiveRecord::Migration
  def self.up
    create_table :articles do |t|
      t.column :title, :string, :limit => 1024
      t.column :text, :text
      t.column :publish_time, :timestamp
      t.column :url, :string, :limit => 1024, :null => false
      t.column :source_id, :integer, :null => false
    end

    execute "alter table articles add constraint fk_articles_sources foreign key (source_id) references sources(id)"
  end

  def self.down
    drop_table :articles
  end
end
