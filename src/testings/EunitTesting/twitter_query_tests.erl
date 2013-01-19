%%%-------------------------------------------------------------------
%%% @author Fan "Bear" Chayi
%%% @copyright (C) 2012, Bear
%%% @doc
%%% This module is used for testing the twitter_query.erl
%%% @end
%%% Manully input the SearchWord  when call this testing function
-module(twitter_query_tests).
-include_lib("eunit/include/eunit.hrl").
get_twitter_test()->
    ?assertMatch({ok,got_results},twitter_query:get_twitter()).
