# This file is autogenerated. Instead of editing this file, please use the
# migrations feature of ActiveRecord to incrementally modify your database, and
# then regenerate this schema definition.

ActiveRecord::Schema.define(:version => 7) do

  create_table "articles", :force => true do |t|
    t.column "title",        :string,   :limit => 1024
    t.column "text",         :string,   :limit => 8388608
    t.column "publish_time", :datetime
    t.column "url",          :string,   :limit => 1024,    :null => false
    t.column "source_id",    :integer,                     :null => false
  end

  add_index "articles", ["source_id"], :name => "index_articles_on_source_id"

  create_table "recommendations", :force => true do |t|
    t.column "user_id",    :integer
    t.column "article_id", :integer
    t.column "score",      :decimal
  end

  create_table "sources", :force => true do |t|
    t.column "url", :string, :limit => 1024
  end

  create_table "users", :force => true do |t|
    t.column "name",            :string
    t.column "hashed_password", :string
    t.column "salt",            :string
  end

end
