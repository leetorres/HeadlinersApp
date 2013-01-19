%%%-------------------------------------------------------------------
%%% @author Fan "Bear" Chayi
%%% @copyright (C) 2012, Bear
%%% @doc
%%% This module is used for testing the mutiple purpose of DB
%%% All the functions in this module are depend on the boss_db sql BIF that has to be execute after the Chicago Boss running.
%%% @end


-module(db_tabletest).
-compile(export_all).
  show_table()->
    io:format("~p~n",[boss_db:execute("show tables;")]).  
  show_column_accounts()->
    io:format("~p~n",[boss_db:execute("show columns from accounts")]).
  show_column_saved_tweets()->
    io:format("~p~n",[boss_db:execute("show columns from saved_tweets")]).
  show_column_searches()->
    io:format("~p~n",[boss_db:execute("show columns from searches")]).
  show_column_tweets()->
    io:format("~p~n",[boss_db:execute("show columns from tweets")]).
  show_column_twitter_users()->
    io:format("~p~n",[boss_db:execute("show columns from twitter_users")]).
  show_table_status()->
    Report=[[(boss_db:execute("check table accounts extended;"))],[boss_db:execute("check table accounts extended;")],[boss_db:execute("check table accounts extended;")],[boss_db:execute("check table accounts extended;")],[boss_db:execute("check table accounts extended;")],[boss_db:execute("check table accounts extended;")]],
    Report.
