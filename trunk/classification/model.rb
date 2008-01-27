module Verditz

  require "activerecord"

  ActiveRecord::Base.establish_connection(:adapter => "mysql", :host => "localhost", :database => "verditz_snapshot", 
                                          :username => "verditz", :password => "verditz", :socket => "/var/run/mysqld/mysqld.sock")

  require "../verditz/app/models/article.rb"
  require "../verditz/app/models/recommendation.rb"
  require "../verditz/app/models/user.rb"
  require "../verditz/app/models/source.rb"
  require "../verditz/app/models/vote.rb"

end
