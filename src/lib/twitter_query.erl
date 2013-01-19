%%%-------------------------------------------------------------------
%%% @author Robin <robin@robin-N53SV>
%%% @copyright (C) 2012, Robin
%%% @doc
%%% This module handles twitter queries.
%%% @end
%%%-------------------------------------------------------------------
-module(twitter_query).
-export([get_twitter/1]).

% @doc
% Makes a httpc:request to twitter to get a number of tweets. The result format will be in json
get_twitter(KeyWord) ->
    inets:start(),
    case httpc:request(get, {build_twitter_url(KeyWord),[]},[{timeout, 6000}],[]) of
	{ok, {_,_,Body}} ->  parse_twitter(Body);
	Other -> {error, Other} 
    end.

% @doc
% Builds a twitter search url/query to use with the RESTful twitter api
build_twitter_url(KeyWord) -> 
    X = strre:replace(KeyWord),
    "http://search.twitter.com/search.json?q=" ++ X.
    
% @doc
% A function that parses twitter data in json to an erlang friendly json structure
parse_twitter(Data) ->
    get_results(mochijson2:decode(Data)).


% @doc 
% Goes through the json structure to get the results we want. In a prop list format
get_results({struct, Data}) ->
    X = proplists:get_value(<<"results">>, Data, not_found),
    get_all(X).

% @doc
% Goes through the prop list of results  recursivly and extract all the data we 
% want and stores it in a database
get_all([]) -> 
    {ok, got_results};
get_all([H|T]) ->
    {struct, X} = H,
    IdStr = check(proplists:get_value(<<"id_str">>, X, not_found)),
    case boss_db:count(tweet, [{id_str, 'matches', IdStr}]) of
	0 ->
	    CreatedAt            = check(proplists:get_value(<<"created_at">>, X, not_found)),
	    FromUser             = check(proplists:get_value(<<"from_user">>, X, not_found)),
	    FromUserIdStr        = check(proplists:get_value(<<"from_user_id_str">>, X, not_found)),
	    FromUserName         = check(proplists:get_value(<<"from_user_name">>, X, not_found)),
	    IsoLanguageCode      = check(proplists:get_value(<<"iso_language_code">>, X, not_found)),
	    ProfileImageUrl      = check(proplists:get_value(<<"profile_image_url">>, X, not_found)),
	    ProfileImageUrlHttps = check(proplists:get_value(<<"profile_image_url_https">>, X, not_found)),
	    Source               = check(proplists:get_value(<<"source">>, X, not_found)),
	    Text                 = check(proplists:get_value(<<"text">>, X, not_found)),
	    ToUserIdStr          = check(proplists:get_value(<<"to_user_id_str">>, X, not_found)),
            Geo                  = "null",

	    Tweet = tweet:new(id, CreatedAt, FromUser, FromUserIdStr, FromUserName,
			      IdStr, IsoLanguageCode, ProfileImageUrl, ProfileImageUrlHttps,
			      Source, Text, ToUserIdStr, Geo, 0),
	    Tweet:save(),
	    get_all(T);
	_Other -> 
	    get_all(T)
    end.

% Checks to see if a value is null, an integer or a binary and transforms to a proper format.
check(X) ->
    case X of
	null -> "null"; % If null returns "null"
	Value when is_integer(Value) -> Value; % If integer returns integer
	Value -> binary_to_list(Value) % If <<"binary">> returns ["list"]
    end.
	    
