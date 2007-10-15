-module(bayes).
-export([train/0, classify/1, test/0, wordcounts/1,testP/0,filter/2]).
-define(bias,2).
-define(min_word_length,3).
-define(WordsPersistentStore,'_words').
-define(blacklist,'stopwords').
-define(hitsDir,"hits").
-define(missesDir,"misses").
%% -define(hitsDir,"hits_small").
%% -define(missesDir,"misses_small").

test() ->
	{T,_} = timer:tc(bayes,testP,[]), 
	T. 

testP()->
	BlackList = blacklist(),
	Hits = wordcounts(?hitsDir),
	Misses = wordcounts(?missesDir),
    Probabilities = create_probabilities(filter(Hits,BlackList), filter(Misses,BlackList)),
    dict:to_list(Probabilities).

filter(WordCounts,BlackList) -> 
	dict:filter(
	  fun(Word,Count) -> 
			  Count > 5, (not lists:member(Word, BlackList))
	  end, 
	  WordCounts).

train() ->
	BlackList = blacklist(),
	Hits = filter(wordcounts(?hitsDir), BlackList),
	Misses = filter(wordcounts(?missesDir), BlackList),
    {ok, words} = create_probabilities(Hits, Misses),
	ets:tab2file(words,?WordsPersistentStore),
	words.

load_store_if_neccessary() ->
	case ets:info(words) of
		undefined -> {ok, words} = ets:file2tab(?WordsPersistentStore),
					 {ok, words, loaded};
		_ -> {ok, words, existed}
	end.


classify(Text) ->
	{ok, words, _} = load_store_if_neccessary(),
    
	Probabilities = lists:filter(
					  fun(P)-> P > 0 end, 
					  lists:map(
						fun(Word) -> get_count(words, Word) end, 
						words_in_string(Text))),

	Product = lists:foldl(fun(A,B)->A*B end,1,Probabilities),
	Score = Product  / (Product + lists:foldl(fun(A, B)->(1-A)*B end, 1, Probabilities)),
	io:format("~f~n",[Score]),
	{bayes,Score}.

create_probabilities(Hits, Misses) ->
	KeysHits = lists:sort(dict:fetch_keys(Hits)),

	KeysMisses = lists:sort(dict:fetch_keys(Misses)),
	Nhits = length(KeysHits),
	Nmisses = length(KeysMisses),
	Words = lists:umerge(KeysHits, KeysMisses),	
	Bags = split_list_into_bags(Words, numbags),
	WordDict = ets:new(words,[set,public,named_table]),
	utils:mapreduce(
	  fun(Pid, Bag) ->
			  lists:map(
				fun(Word) -> 
						Pid ! 
							{Word,
							 probability(
							   get_item(Word, Hits), 
							   get_item(Word, Misses) * ?bias, 
							   Nhits,
							   Nmisses,
							   Word)} end, 
				Bag) end,
	  fun(Key,Values,_A) -> 
			  [Value | _] = Values,
			  ets:insert(WordDict,{Key, Value}) end,
	  nothing, Bags),
	{ok, words}.

probability(Hit, Miss, Nhits, Nmisses, Word) ->
	P = (Hit/Nhits)/((Hit/Nhits) + (Miss/Nmisses)),
	lists:max([lists:min([P,0.99]),0.01]). 

	

split_list_into_bags(L, N) when length(L) < N ->
	[L];

split_list_into_bags(L,N) ->
	{L1, L2} = lists:split(N, L),
	[L1] ++ split_list_into_bags(L2,N).


blacklist() ->
	[string:to_lower(Word) || Word <- words_in_string(read_file(?blacklist))].

%build a dictionary of all the text in Directory
wordcounts(Directory) ->
	Files = lib_find:files(Directory, "*", false),
	Texts = lists:map(fun(File)-> string:to_lower(read_file(File)) end,Files), 
	dict:from_list(utils:mapreduce(
					 fun(Pid, Text) -> 
							 SendWord = fun(Word) -> Pid ! {Word, 1} end,
							 lists:map(SendWord, words_in_string(Text)) end,
					 fun(Key, Values, A) -> 
							 [{Key, length(Values)} | A] end, 
					 [], Texts)).

read_file(File) ->
    case file:read_file(File) of
		{ok, Bin} -> binary_to_list(Bin);
		_         -> []
    end.

words_in_string(String) ->
	case regexp:split(string:to_lower(String), "[^a-zA-Z]") of
		{ok,Words} -> lists:filter(fun(X) -> length(X) > 0 end, Words);
		_ -> []
	end.


get_item(Key, Dict) ->
	case dict:find(Key,Dict) of
		error -> 0;
		{ok, Value} -> Value end.

%    


	
get_count(Dict, W) ->
	case ets:lookup(Dict,W) of 
		[{_,X}] -> X;
	    [] -> 0 end.

	
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


