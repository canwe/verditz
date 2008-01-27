module Verditz

  class Classifier


    def initialize featureselector
      @featureselector = featureselector
      @feature_counts = Hash.new{|h,k|h[k] = {}}
      @category_counts = Hash.new{|h,k|h[k] = 0}
      @thresholds = {}
    end

    def set_threshold cat, value
      @thresholds[cat.to_sym] = value
    end

    def get_threshold cat
      if @thresholds.has_key?(cat.to_sym)
        return @thresholds[cat]
      else
        return 1.0 
      end
    end
    
    def find_features item
      @featureselector.call(item)
    end

    def train item, cat
      find_features(item).each do |feature|
        inc_feature cat, feature
      end
      inc_category cat
    end

    def feature_count cat, feature
      if @feature_counts.has_key? feature and @feature_counts[feature].has_key? cat
        @feature_counts[feature][cat.to_sym]
      else
        0
      end
    end

    def category_count cat
      @category_counts[cat.to_sym]
    end

    def categories 
      @category_counts.keys
    end

    def num_documents
      @category_counts.values.inject{|sum,n|sum + n}
    end

    def features
        @feature_counts.keys
    end


    def probabilities
      probs = {}
      for feature, counts in @feature_counts
        catprobs = {}
        counts.keys.each{|cat| catprobs[cat] = feature_probability(cat, feature)}
        probs[feature] = catprobs
      end
      probs
    end

    def feature_probability cat, feature, weight=1.0, bias=0.5
      cat = cat.to_sym
      basic_prob = basic_probability(cat, feature)
      total_count = categories.collect{|c| feature_count(c, feature)}.inject{|sum,n| sum + n}.to_f
      ( ((weight*bias) + (total_count*basic_prob)) / (weight + total_count) )
    end

    private

    def inc_feature cat, feature
      cat = cat.to_sym
      @feature_counts[feature][cat] ||= 0
      @feature_counts[feature][cat] += 1
    end

    def inc_category cat
      @category_counts[cat.to_sym] += 1
    end

    def basic_probability cat, feature
      cat = cat.to_sym
      return 0.0 if category_count(cat) == 0
      return feature_count(cat, feature).to_f / category_count(cat).to_f
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

    def test_text_words
      words = FeatureSelection::words.call(GOOD_TEXTS.first)
      assert (not words.empty?)
    end

    def test_classifier_train
      classifier = Classifier.new FeatureSelection::words
      GOOD_TEXTS.each{|text| classifier.train(text, :good)}
      BAD_TEXTS.each{|text| classifier.train(text, :bad)}

      p classifier.probabilities

      assert classifier.num_documents == 5
      assert classifier.categories == [:good, :bad]
      assert classifier.features.size == 63

    end


  end

end
