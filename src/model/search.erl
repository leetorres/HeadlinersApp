%%%-------------------------------------------------------------------
%%% @author Lee
%%% @copyright (C) 2012, Headliners
%%% @doc
%%% This module does validation tests on the SearchText.
%%% @end
%%%-------------------------------------------------------------------
-module(search, [Id, SearchText, Latitude, Longitude, City, Country]).
-compile(export_all).

% @doc
% Function responsible for deleting data
delete() ->
    boss_db:delete(id()).

% @doc
% Function that does validation on the search strings. Makes sure
% that there are no blank search strings, and no strings larger than
% 140 characters, the standard size of a tweet
validation_tests()->
    [{fun() ->
	      length(SearchText) > 0 end,
      "Search request  must be non-empty!"},
     {fun() ->
	      length(SearchText) =< 140 end,
      "Search request is too long!\nMust be less than 141 characters."}].

%% MAY BE USED LATER
%% Hook for updating # of searches counter so
%% we can post list of most popular searches:
%before_create() ->
%    case boss_db:find(search, [{search_text, 'equals', SearchText}]) of
%	[AlreadyExisting] -> % Case that the search term already exists in the DB.
%	    OldCount = AlreadyExisting:count(),
%	    {ok, AlreadyExisting:set(count, OldCount + 1)};
%	[] -> % Case that the search term does not yet exist in the DB.
%	    ok
%    end.
