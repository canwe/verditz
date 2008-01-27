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
require "classify"
class TC_MyTest < Test::Unit::TestCase

  def setup
  end
  
  def teardown
  end

  def test_training
    
  end

  def test_model
    assert((User.find :all).size == 3)
    assert((Recommendation.find :all).empty?, "expected no recommendations")
    assert((Vote.find :all).size == 169)
  end

end
