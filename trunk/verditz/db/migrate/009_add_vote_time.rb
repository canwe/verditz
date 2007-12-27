class AddVoteTime < ActiveRecord::Migration
  def self.up
    add_column :votes, :createtime, :datetime, :default => Time.now
  end

  def self.down
    remove_column :votes, :createtime
  end
end
