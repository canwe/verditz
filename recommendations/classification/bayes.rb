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

  module Strategy

    class NaiveBayes

      def initialize classifier
        @classifier = classifier
      end
      
      def probability cat, item
        catprob = @classifier.category_count(cat) / @classifier.num_documents.to_f
        docprob = document_probability(cat, item)
        (docprob * catprob)
      end

      def document_probability cat, item
        features = @classifier.find_features(item)
#        features.inject(1){ |r,f|r * @classifier.feature_probability(cat,f) }
        p = 1
        features.each{|f| p *= @classifier.feature_probability(cat,f) }
        p
      end

    end
    
  end
end


if $0 == __FILE__

  require "test/unit"
  require "features"
  require "classifier"
  class TheTest < Test::Unit::TestCase
    include Verditz
    GOOD_TEXTS = ["Python and Scheme have different philosophies when it comes to strings.",
                  "Welcome to the Ruby Standard Library Documentation collection, brought to you by the ruby-doc project. Whether you are browsing online or offline, we hope that your use of Ruby's standard library will become more productive as a result of this effort.", 
                  "After releasing PSP Stackless Python 2.5.1 last week, I started working on a project site to host the files, sources, documentation and issues for my Stackless Python port for the PSP."]

    BAD_TEXTS = ["Obama 55%, Clinton 27%, Edwards 18% in South Carolina.", 
                 "Ron Paul is our man.", "Bush on war against terror", "
Among the normal rats, the animals that consumed moderate amounts of alcohol fared better on both tests compared with the teetotalers. Rats on a heavy alcohol diet did not do well on object recognition (and, in fact, showed signs of neurotoxicity), but they performed better than their normal brethren on the emotional memory task."]

    def test_bayes
      classifier = Classifier.new(FeatureSelection::WordSelector, Strategy::NaiveBayes)
      GOOD_TEXTS.each{|text| classifier.train(text, :good)}
      BAD_TEXTS.each{|text| classifier.train(text, :bad)}

      #we want to classify as good only if we are really sure it is a good text
      classifier.set_threshold(:good, 4)

      assert classifier.classify("Python is a Scheme when it comes to strings! Ron Paul. Ron Paul")[:guess] == :good
      assert classifier.classify("Ron Paul. Ron Paul")[:guess] == :bad

      assert classifier.classify("Karabonga")[:guess] == :bad

      assert classifier.classify("Python is a Scheme when it comes to strings! Ron Paul. Ron Paul")[:guess] == :good
      
      difficult_text = "Ron Paul & Hillary Clinton do Python & Ruby."
      assert classifier.classify(difficult_text)[:guess] == :unknown
    end
  end
end
