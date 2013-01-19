%%%-------------------------------------------------------------------
%%% @author Chayi "Bear" Fan
%%% @copyright (C) 2012, Headliners
%%% @doc
%%% This module gets user information from twitter.
%%% @end
%%%-------------------------------------------------------------------

-module(intwitter_query).
-compile(export_all).

% @doc
% Takes a keyword and makes a httpc:request to twitter for getting user information.
% KeyWord is the twitter users id.
get_intwitter(KeyWord) ->
    ssl:start(),
    inets:start(),

    case httpc:request(get, {build_intwitter_url(KeyWord),[]},[{timeout, 2000}],[]) of 
	{ok, {_,_,Body}} -> parse_intwitter(Body);
	Other -> {error, Other} 
    end.

% @doc
% Creating a correct url to use in the httpc:request
build_intwitter_url(KeyWord) -> 
    X = strre:replace(KeyWord),
    "https://api.twitter.com/1/users/lookup.json?user_id=" ++ X ++ "&include_entities=true".

% @doc
% Parsing the data.
parse_intwitter(Results) ->   
    get_all(mochijson2:decode(Results)).

% @doc
% Exporting all the data from the json structure and stores it in a database.
% Will check if the user is already in the database, if it is it wont store anything.
get_all([]) ->
    {ok, got_results};
get_all(Results) ->  
    [{struct,X}]=Results,
    IdStr=check(proplists:get_value(<<"id_str">>,X,not_found)),
    case boss_db:count(twitter_user,[{id_str,'matches',IdStr}]) of
	0->
	    Location        = check(proplists:get_value(<<"location">>,X,not_found)),
	    Friends         = check(proplists:get_value(<<"friends_count">>,X,not_found)),
	    FollowersCount  = check(proplists:get_value(<<"followers_count">>,X,not_found)),
	    ListedCount     = check(proplists:get_value(<<"listed_count">>,X,not_found)),
	    CreatedAt       = check(proplists:get_value(<<"created_at">>,X,not_found)),
	    Description     = check(proplists:get_value(<<"description">>,X,not_found)),
	    Url             = check(proplists:get_value(<<"url">>,X,not_found)), 
 
	    Twitter_User=twitter_user:new(id, IdStr, Location, Friends, FollowersCount, 
					  ListedCount, CreatedAt, Description, Url),
    
	    Twitter_User:save();    
	_Other->
	    wrong
    end.
					       
% @doc
% Checks all the return values from the json structure to make sure its formatted properly.
check(X) ->
    case X of
	null -> "null"; % Turns the atom null into the string "null"
	Value when is_integer(Value) -> Value; % If its an integer it will return it as an integer 
	Value -> binary_to_list(Value) % Otherwise will return the transform from a <<"binary">> to a ["list"]
    end.
