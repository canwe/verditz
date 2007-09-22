-module(bayes).
-compile(export_all).

words(Bin) ->
    {ok, Words} = regexp:split(binary_to_list(Bin), "[^a-zA-Z]"),
    lists:map(fun(X) -> X end, lists:filter(fun(X) -> length(X) > 0 end, Words)).


wordcounts(Class, Words) ->
    Dict = ets:new(Class, [set, private]),
    lists:foreach(fun(X) ->
        case ets:insert_new(Dict, {list_to_binary(X), 1}) of
            false -> ets:update_counter(Dict, list_to_binary(X), 1);
            true -> true
        end
    end, Words),
    Dict.

count(Dict, W) ->
	case ets:lookup(Dict,W) of 
		[{_,X}] -> X;
	    [] -> 0 end.

word_is_hit(Word) ->  
	W = list_to_binary(Word),
	HitCorpus = words(list_to_binary("Hallo Welt. Hallo Hallo Hallo Free Software!")),
	MissCorpus = words(list_to_binary("Penis Enlargement. Free Girls! Free Gold, Free Nazi-Underware Free Free Free Free Free Fish")),
	HitCounts = wordcounts(good,HitCorpus),
	MissCounts  = wordcounts(bad,MissCorpus),
	Hit = count(HitCounts,W) * 2,
	Miss = count(MissCounts,W),
	(Hit/length(HitCorpus))/((Hit/length(HitCorpus)) + (Miss/length(MissCorpus)) + 0.001 ).
