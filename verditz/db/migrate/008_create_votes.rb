class CreateVotes < ActiveRecord::Migration
  def self.up
    create_table :votes do |t|
      t.column :user_id, :integer
      t.column :article_id, :integer
      t.column :value, :integer
    end
  end

  def self.down
    drop_table :votes
  end
end
