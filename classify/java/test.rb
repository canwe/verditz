
if ARGV.shift == "C"

  def classify_as messagefile, klass
    classpath = 'lib/snowball.jar:lib/weka.jar:bin'
    system("java -cp #{classpath} ac.at.tuwien.dbai.verditz.classify.MessageClassifier -o classifier -m #{messagefile} -c #{klass}")
  end


  Dir['data/misses/*'].each do |filename| 
    classify_as filename, "miss"
  end

  Dir['data/hits/*'].each do |filename| 
    classify_as filename, "hit"
  end

else
  
  def classify messagefile
    classpath = 'lib/snowball.jar:lib/weka.jar:bin'
    `java -cp #{classpath} ac.at.tuwien.dbai.verditz.classify.MessageClassifier -o classifier -m #{messagefile}`
  end


  ARGV.each do |filename|
    p filename
    classify filename
  end

end
