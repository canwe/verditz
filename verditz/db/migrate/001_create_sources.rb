class CreateSources < ActiveRecord::Migration
  def self.up
    create_table :sources do |t|
      t.column :url, :string, :limit => 1024
    end
  end

  def self.down
    drop_table :sources
  end
end
