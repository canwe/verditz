erlc bayes.erl 
erl -noshell -s bayes train_hit "Hits" -s init stop
erl -noshell -s bayes train_miss "Misses" -s init stop
