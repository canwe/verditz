
def classify messagefile, klass
  classpath = 'lib/snowball.jar:lib/weka.jar:bin'
  system("java -cp #{classpath} ac.at.tuwien.dbai.verditz.classify.MessageClassifier -o classifier -m #{messagefile} -c #{klass}")
end


Dir['data/misses/*'].each do |filename| 
  classify filename, "miss"
end

Dir['data/hits/*'].each do |filename| 
  classify filename, "hit"
end
