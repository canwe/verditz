#!/opt/local/bin/ruby -w 
words = {}
words.default = 0

ARGF.read.scan(/[a-zA-Z]+/).each do |word| 
  if word.size > 2
    words[word] += 1
  end
end

words.keys.sort.each do |word| 
  if words[word] > 2
    puts "#{word},#{words[word]}"
  end
end
