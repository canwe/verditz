-module(tcs).
-export([start/0, train_hit/1, train_miss/1]).

start() ->
	register(tcs, spawn(fun() -> loop() end)).

rpc(Q) ->
	tcs ! {self(), Q},
	receive
		{tcs, Result} ->
			Result
	end.

train_hit(Text) ->
	rpc({train,hits,Text}).

train_hits_from_file() ->
	rpc({train,hitsfile}).

train_misses_from_file() ->
	rpc({train,missesfile}).


train_miss(Text) ->
	rpc({train,misses,Text}).

loop() ->
	receive
		{From, {train, hits, Text}} ->
			P = bayes:train_hit(Text),
			From ! {tcs, P}, 
			loop();
		{From, {train, misses, Text}} ->
			P = bayes:train_miss(Text),
			From ! {tcs, P}, 
			loop();
		{From, {train, hitsfile}} -> 
			P = bayes:train_hitsfile(),
			From ! {tcs, P}, 
			loop();
		{From, {train, missesfile}} ->
			P = bayes:train_missesfile(),
			From ! {tcs, P}, 
			loop()
	end.
