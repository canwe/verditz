require "classifier"
require "delegate"
module Verditz
  
  class NaiveBayesClassifier < DelegateClass(Classifier)
    
    def initialize find_features
      @classifier = Classifier.new find_features
      super(@classifier)
    end

    def classify item, default = :unknown
      catprobs= categories.collect{ |cat| {:cat => cat, :prob => probability(cat,item)} }
      best = catprobs.max{|a,b|a[:prob] <=> b[:prob]}
      if below_threshold? catprobs, best
        return default
      else
        return best[:cat]
      end
    end

    def document_probability cat, item
      features = find_features(item)
      features.inject(1){ |r,f|r * feature_probability(cat,f) }
    end

    def probability cat, item
      catprob = category_count(cat) / num_documents.to_f
      docprob = document_probability(cat, item)
      (docprob * catprob)
    end

    def below_threshold? catprobs, best
      for item in catprobs.reject{|x|x != best}
        if (item[:prob] * get_threshold(best[:cat])) > best[:prob]
          return true
        end
      end
      return false
    end

  end

end


if $0 == __FILE__

  require "test/unit"
  require "featureselection"

  class TheTest < Test::Unit::TestCase
    include Verditz
    GOOD_TEXTS = ["Python and Scheme have different philosophies when it comes to strings.",
                  "Welcome to the Ruby Standard Library Documentation collection, brought to you by the ruby-doc project. Whether you are browsing online or offline, we hope that your use of Ruby's standard library will become more productive as a result of this effort."]
    BAD_TEXTS = ["Obama 55%, Clinton 27%, Edwards 18% in South Carolina.", "Ron Paul is our man.", "Bush on war against terror"]

    def test_classifier_train
      classifier = NaiveBayesClassifier.new FeatureSelection::words
      GOOD_TEXTS.each{|text| classifier.train(text, :good)}
      BAD_TEXTS.each{|text| classifier.train(text, :bad)}

#      classifier.set_threshold(:good, 1)

      assert classifier.classify("Python is a Scheme when it comes to strings! Ron Paul. Ron Paul") == :good
      assert classifier.classify("Ron Paul. Ron Paul") == :bad

#      p classifier.classify("Karabonga")
#      assert classifier.classify("Karabonga") == :unknown

    end


  end

end
