task :compile => :compile_tcs
task :test => [:test_bayes, :test_tcs]
task :default => [:compile_tcs, :test] do 
  puts "done."
end

task :compile_bayes do 
  `erlc bayes.erl` 
end

task :compile_tcs => :compile_bayes do 
  `erlc tcs.erl` 
end


task :test_tcs => :compile_tcs do
  `erl -noshell -sname bobo@localhost -s tcs start -s tcs train_hit "Hallo Welt" -s init stop`
end

task :test_bayes => :compile_bayes do
  `erl -sname bobo@localhost -s bayes classify "Hallo Welt" -s init stop`
end




