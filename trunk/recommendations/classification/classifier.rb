#     verditz classification framework.
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

  class Classifier

    def initialize featureselector, strategy
      @strategy = strategy.new(self)
      @featureselector = featureselector.class == Class ? featureselector.new : featureselector
      @feature_counts = Hash.new{|h,k|h[k] = {}}
      @category_counts = Hash.new{|h,k|h[k] = 0}
      @thresholds = {}
    end

    def guess item, default = {:guess => :unknown, :score => 1}
      classify(item,default)[:guess]
    end

    def classify item, default = {:guess => :unknown, :score => nil}
      catprobs= categories.collect{ |cat| {:guess => cat, 
                                           :score => @strategy.probability(cat,item)} }
      best = catprobs.max{|a,b| a[:score] <=> b[:score]}
      if below_threshold? catprobs, best
        return default
      else
        return best
      end
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
      @featureselector.extract(item)
    end

    def train item, cat
      find_features(item).each do |feature|
        inc_feature cat, feature
      end
      inc_category cat
    end

    def feature_count cat, feature
      if @feature_counts.has_key?(feature) and @feature_counts[feature].has_key? cat.to_sym
        return @feature_counts[feature][cat.to_sym]
      else
        return 0.0
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

    #this is the weighted probability that a feature belongs to a category
    def feature_probability cat, feature, weight=1.0, guess=0.5
      cat = cat.to_sym
      basic_prob = basic_probability(cat, feature)
      total_count = categories.collect{|c| feature_count(c, feature)}.inject{|sum,n| sum + n}.to_f
      p = ( ((weight * guess) + (total_count * basic_prob)) / (weight + total_count) )
      p
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

    def below_threshold? catprobs, best
      for item in catprobs.reject{|x|x == best}
        if (item[:score] * get_threshold(best[:guess])) > best[:score]
          return true
        end
      end
      return false
    end


  end
    
end


if $0 == __FILE__

  require "test/unit"
  require "features"

  class TheTest < Test::Unit::TestCase
    include Verditz
    GOOD_TEXTS = ["Python and Scheme have different philosophies when it comes to strings.",
                  "Welcome to the Ruby Standard Library Documentation collection, brought to you by the ruby-doc project. Whether you are browsing online or offline, we hope that your use of Ruby's standard library will become more productive as a result of this effort."]
    BAD_TEXTS = ["Obama 55%, Clinton 27%, Edwards 18% in South Carolina.", "Ron Paul is our man.", "Bush on war against terror"]

    class DummyFeatureSelector
      
      #extract a list of features from a corpus.
      def extract corpus
        corpus.split(/\b/).uniq.collect{ |word| word.strip.downcase}.
             select{ |word| word.size > 1 && word.size < 20 }
      end
    end

    class DummyStrategy

      #backreference allows strategies to 
      #access instance methods of the classifier
      def initialize classifier
      end

      #calculate the probability that item 
      #belongs to category.
      def probability category, item
        return 0.5
      end
    end


    def test_classifier_train
      classifier = Classifier.new DummyFeatureSelector, DummyStrategy
      GOOD_TEXTS.each{|text| classifier.train(text, :good)}
      BAD_TEXTS.each{|text| classifier.train(text, :bad)}

      assert classifier.num_documents == 5
      assert (classifier.categories & [:good, :bad]).size == 2 
      assert classifier.features.size == 63
    end


  end

end
