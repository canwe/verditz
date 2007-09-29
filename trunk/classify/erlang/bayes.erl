-module(bayes).
-export([train/0, classify/1, test/0, wordcounts/1,testP/1]).
-define(bias,2).
-define(min_word_length,3).

-define(hitsDir,"hits").
-define(missesDir,"misses").
%% -define(hitsDir,"hits_small").
%% -define(missesDir,"misses_small").

test() ->
    lists:foreach(
	  fun(_N) -> 
			  L = lists:map(fun(X) -> 
								{T,_} = timer:tc(bayes,testP,[X]), T end, 
						[1000, 2000, 5000, 10000, 100000, 1000000]),
			  io:format("~p~n",[L]) end,
	  lists:seq(1,100)).

testP(N)->
	Hits = wordcounts(?hitsDir),
	Misses = wordcounts(?missesDir),
	Misses.
%    Probabilities = create_probabilities(filter(Hits), filter(Misses), N),
%	dict:to_list(Probabilities).

filter(WordCounts) -> 
	dict:filter(fun({_,Count}) -> Count > 5 end, WordCounts).

train() ->
	Hits = wordcounts(?hitsDir),
	Misses = wordcounts(?missesDir),
    Probabilities = create_probabilities(Hits, Misses, 10),
	{bayes,ok}.
	

get_item(Key, Dict) ->
	case dict:find(Key,Dict) of
		error -> 0;
		{ok, Value} -> Value end.
					 
									 

create_probabilities(Hits, Misses, numbags) ->
	KeysHits = lists:sort(dict:fetch_keys(Hits)),
	KeysMisses = lists:sort(dict:fetch_keys(Misses)),
	Nhits = length(KeysHits),
	Nmisses = length(KeysMisses),
	Words = lists:umerge(KeysHits, KeysMisses),	
	Bags = split_list_into_bags(Words, numbags),
	WordDict = ets:new(words,[set,public,named]),
	phofs:mapreduce(
	  fun(Pid, Bag) ->
			  lists:map(
				fun(Word) -> 
						Pid ! 
							{Word,
							 probability(
							   get_item(Word, Hits), 
							   get_item(Word, Misses), 
							   Nhits,
							   Nmisses,
							   Word)} end, 
				Bag) end,
	  fun(Key,Values,_A) -> 
			  [Value | _] = Values,
			  ets:insert(WordDict,{Key, Value}) end,
	  nothing, Bags),
	{bayes, words}.

probability(Hit, Miss, Nhits, Nmisses, Word) ->
	P = (Hit/Nhits)/((Hit/Nhits) + (Miss/Nmisses)),
	lists:max([lists:min([P,0.99]),0.1]). 

	

split_list_into_bags(L, N) when length(L) < N ->
	[L];

split_list_into_bags(L,N) ->
	{L1, L2} = lists:split(N, L),
	[L1] ++ split_list_into_bags(L2,N).

classify(Text) ->
	0.


wordcounts(Directory) ->
	Files = lib_find:files(Directory, "*", false),
	dict:from_list(phofs:mapreduce(
	  fun(Pid, File) -> 
			  SendWord = fun(Word) -> Pid ! {httpd_util:to_lower(Word), 1} end,
			  lists:map(SendWord, words_in_string(read_file(File))) end,
	  fun(Key, Values, A) -> 
			  [{Key, length(Values)} | A]
	  end, 
	  [], Files)).

read_file(File) ->
    case file:read_file(File) of
		{ok, Bin} -> binary_to_list(Bin);
		_         -> []
    end.

words_in_string(String) ->
	case regexp:split(httpd_util:to_lower(String), "[^a-zA-Z]") of
		{ok,Words} -> lists:filter(fun(X) -> length(X) > 0 end, Words);
		_ -> []
	end.

%    

	
%time with ets: {47 62 56 04,18}
% time with dict: 48258832,
% time with dict and 100 cpus: 76511359
%% word_is_hit(Word) ->  
%% 	case wordcounts(Word) of 
%% 		{{hits, 0}, {misses, 0}} -> 0.4;
%% 		{{hits, Hit}, {misses, Miss}} -> 
%% 			Nhits = ets:info(hits,size),
%% 			Nmiss = ets:info(misses,size),
%% 			P = (Hit/Nhits)/((Hit/Nhits) + (Miss/Nmiss)),
%% 			lists:max([lists:min([P,0.99]),0.1]) 
%% 	end.
%% wordcounts(W) ->
%% 	Word = list_to_binary(W),
%% 	{{hits, wordcount(hits,Word)},{misses, wordcount(misses,Word)}}.





%% wordcounts_old(Directory) ->
%% 	Files = lib_find:files(Directory, "*", false),
%% 	Dict = ets:new(list_to_atom, [set, public]),
%% 	phofs:mapreduce(
%% 	  fun(Pid, File) -> 
%% 			  F = fun(Word) -> Pid ! {httpd_util:to_lower(Word), 1} end,
%% 			  lib_misc:foreachWordInFile(File, F) end,
%% 	  fun(Key, Values, _) -> ets:insert(Dict,{Key, length(Values)}) end, 
%% 	  nothing, Files),
%% 	Dict.
