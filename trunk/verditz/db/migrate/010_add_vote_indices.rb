class AddVoteIndices < ActiveRecord::Migration
  def self.up
    add_index :votes, :user_id
    add_index :votes, :article_id
  end

  def self.down
  end
end
