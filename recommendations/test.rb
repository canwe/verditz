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

require "test/unit"
require "classification/nbayes"
class TC_MyTest < Test::Unit::TestCase

  def setup
  end
  
  def teardown
  end


  def test
    
  end


  def test_training

    require "mysql"
    m = Mysql.new("localhost", "root", "", "verditz_snapshot")
    res = m.query("select * from votes limit 5")
    fields = res.fetch_fields.collect{|f|f.name}
    puts fields.join("\t")
    res.each_hash do |row|
      p row
    end    
  end

end
