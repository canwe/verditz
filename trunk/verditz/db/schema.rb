# This file is autogenerated. Instead of editing this file, please use the
# migrations feature of ActiveRecord to incrementally modify your database, and
# then regenerate this schema definition.

ActiveRecord::Schema.define(:version => 4) do

  create_table "articles", :force => true do |t|
    t.column "title",        :string,   :limit => 1024
    t.column "text",         :text
    t.column "publish_time", :datetime
    t.column "url",          :string,   :limit => 1024, :null => false
    t.column "source_id",    :integer,                  :null => false
  end

  create_table "recommendations", :force => true do |t|
  end

  create_table "sources", :force => true do |t|
    t.column "url", :string, :limit => 1024
  end

  create_table "users", :force => true do |t|
  end

end
