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

require "delegate"
require "classifier"
require "bayes"
require "features"
module Verditz
  
  class NaiveBayesClassifier < DelegateClass(Classifier)
    
    def initialize
      @classifier = Classifier.new FeatureSelection::WordSelector, Strategy::NaiveBayes
      super @classifier
    end

  end

end

if $0 == __FILE__
  Verditz::NaiveBayesClassifier.new
end
