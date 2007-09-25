-module(bayes).
-export([
		 train/0,
		 train_hit/1,
		 train_miss/1, 
		 train_hitsfile/0,
		 train_missesfile/0,
		 init/0,
		 classify/1,
		 classify_file/1,
		 test/0]).
-define(bias,2).
-define(min_word_length,3).


%-include_lib("xmerl/include/xmerl.hrl").
%-export([main/1]).

% train_hit:
% Input: String 
% Output: void
% Process:
% - load ets table from file (if exists)
% - split string into word => count map
% - merge word - count map into ets table
% - save ets table to file

% train_miss: like hit ...
%

% classify_many 
% Input: Array of Strings (Texte, jeweils erste zeile ist titel/id)
% Output: map: Id => Probability
% Process:
% - foreach Text:
%     Id = first line
%     spawn new process
%     Map = fun({Id,Text}) -> ... {ok,Id,P} end  
%     Reduce  = fun({Id,Text}) -> ... {ok,[{Id,P}]} end  
%     


train_hit(Text) ->
	train(hits, Text).

train_miss(Text) ->
	train(misses, Text).

train() ->
	train_hitsfile(),
	train_missesfile(),
	{bayes,ok}.

train_hitsfile() ->
	train_hit(binary_to_list(read_file("raw_hits"))).

train_missesfile() ->
	train_miss(binary_to_list(read_file("raw_misses"))).

train(Class, Text) ->
	Table = create_wordcounts_table(Class,Text),
	ets:tab2file(Table, atom_to_list(Class)).

init() ->
	{ok, _} = ets:file2tab('hits'),
	{ok, _} = ets:file2tab('misses').

test() ->
	Text = binary_to_list(read_file('raw_hits')),
	Probabilities = lib_misc:pmap(fun(Word) -> word_is_hit(Word) end, words(Text)).

classify_file(Filename) ->
	classify(binary_to_list(read_file(Filename))).

classify(Text) ->
	Probabilities = filter(interesting_word, 
						   lib_misc:pmap(fun(Word) -> word_is_hit(Word) end, words(Text))),
 	Product = lists:foldl(fun(A,B)->A*B end,1,Probabilities),
	Score = Product  / (Product + lists:foldl(fun(A,B)->(1-A)*B end,1,Probabilities)),
	Score.

interesting_word(P) ->
	math:abs(P-0.5) > 0.1.

read_file(Filename) -> 
	{ok, Bin} = file:read_file(Filename),
    Bin.

words(Text) ->
    {ok, Words} = regexp:split(httpd_util:to_lower(Text), "[^a-zA-Z]"),
    lists:map(fun(X) -> X end, lists:filter(fun(X) -> length(X) > 0 end, Words)).

create_wordcounts_table(Class, Texts) ->
	Words = words(Texts),
    Dict = ets:new(Class, [set, public, named_table]),
    lists:foreach(fun(X) ->
        case ets:insert_new(Dict, {list_to_binary(X), 1}) of
            false -> ets:update_counter(Dict, list_to_binary(X), 1);
            true -> true
        end
    end, Words),
    Dict.

word_is_hit(Word) ->  
	W = list_to_binary(Word),
   	Hit = wordcount(hits,W) * ?bias,
	Miss = wordcount(misses,W),
	Nhits = ets:info(hits,size),
	Nmiss = ets:info(misses,size),
	P = (Hit/Nhits)/((Hit/Nhits) + (Miss/Nmiss)),
	lists:max([lists:min([P,0.99]),0.1]).
	
wordcount(Dict, W) ->
	case ets:lookup(Dict,W) of 
		[{_,X}] -> X;
	    [] -> 0.1 end.
