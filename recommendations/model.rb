# verditz classification framework
#
#     Copyright (C) 2008  Benjamin Ferrari
#
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <http://www.gnu.org/licenses/>.

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
