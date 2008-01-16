class AllowArticlesWithoutSource < ActiveRecord::Migration
  def self.up
    #change_column :articles, :source_id, :integer, :null => true, :default => nil
    #rails bug: http://dev.rubyonrails.org/ticket/8115
    execute "ALTER TABLE articles ALTER source_id DROP NOT NULL"
  end

  def self.down
    #change_column :articles, :source_id, :integer, :null => false
    #rails bug: http://dev.rubyonrails.org/ticket/8115
    execute "DELETE FROM articles WHERE source_id is NULL"
    execute "ALTER TABLE articles ALTER source_id SET NOT NULL"
  end
end
