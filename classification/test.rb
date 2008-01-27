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
