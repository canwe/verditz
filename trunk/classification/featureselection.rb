module Verditz

  module FeatureSelection
    #these methods return procs that are used for feature selection of various input data.
    #for example, the words seletor takes a text corpus and returns a list of unique words.
    
    def self.words 
      proc{|text| text.split(/\b/).uniq.collect{ |word| word.strip.downcase}.
                     select{ |word| word.size > 1 && word.size < 20 } }
    end
  end
end
