-module(bayes).
-compile(export_all).
-define(bias,2).
-define(min_word_length,3).
%-export([main/1]).

main([Arg]) ->
	Hits = create_wordcounts_table(read_file("Hits")),
	Misses = create_wordcounts_table(read_file("Misses")),
	Words = words(list_to_binary(atom_to_list(Arg))),
	Probabilities = lists:map(fun(Word) -> word_is_hit(Word,Hits,Misses) end, Words),
 	Product = lists:foldl(fun(A,B)->A*B end,1,Probabilities),

	Score = Product / (Product + lists:foldl(fun(A,B)->(1-A)*B end,1,Probabilities)),

	io:format("~w~n",[Score]).

read_file(Filename) -> 
	{ok, Bin} = file:read_file(Filename),
    Bin.

words(Bin) ->
    {ok, Words} = regexp:split(binary_to_list(Bin), "[^a-zA-Z]"),
    lists:map(fun(X) -> X end, lists:filter(fun(X) -> length(X) > 0 end, Words)).

create_wordcounts_table(Texts) ->
%%	Words = lists:foldl(fun(A,B) -> lists:append(words(A),B) end, [], Texts),
	Words = words(Texts),
    Dict = ets:new(table, [set, private]),
    lists:foreach(fun(X) ->
        case ets:insert_new(Dict, {list_to_binary(X), 1}) of
            false -> ets:update_counter(Dict, list_to_binary(X), 1);
            true -> true
        end
    end, Words),
    Dict.

wordcount(Dict, W) ->
	case ets:lookup(Dict,W) of 
		[{_,X}] -> X;
	    [] -> 0.1 end.

word_is_hit(Word, HitCounts, MissCounts) ->  
	W = list_to_binary(Word),
   	Hit = wordcount(HitCounts,W) * ?bias,
	Miss = wordcount(MissCounts,W),
	Nhits = ets:info(HitCounts,size),
	Nmiss = ets:info(MissCounts,size),
	P = (Hit/Nhits)/((Hit/Nhits) + (Miss/Nmiss)),
	lists:max([lists:min([P,0.99]),0.1]).
	
