%%%-------------------------------------------------------------------
%%% @author Fan "Bear" Chayi
%%% @copyright (C) 2012, Bear
%%% @doc
%%% This module is used for testing integrity of DB
%%% All the functions in this module are depend on the boss_db sql BIF that has to be execute after the Chicago Boss running.
%%% @end

-module(db_integrationtest).
-compile(export_all).
  duplication_for_accounts()->
  case length(boss_db:execute("select * from accounts group by id,email having count(*)>1;")) of
     0-> [];
     _->duplication_situation_detected
end.
  duplication_for_googles()->  
  case length(boss_db:execute("select * from accounts group by id,link having count(*)>1;")) of
     0-> [];
     _->duplication_situation_detected
end.
  duplication_for_saved_tweets()->  
    case length(boss_db:execute("select * from accounts group by id,id_str having count(*)>1;")) of
     0-> [];
     _->duplication_situation_detected
end.
  duplication_for_searches()->
   case length(boss_db:execute("select * from accounts group by id having count(*)>1;")) of
     0-> [];
     _->duplication_situation_detected
end.
  duplication_for_tweets()-> 
    case length(boss_db:execute("select * from accounts group by id,id_str having count(*)>1;")) of
     0-> [];
     _->duplication_situation_detected
end.
  duplication_for_twitter_users()->
    case length(boss_db:execute("select * from accounts group by id,id_str having count(*)>1;")) of
     0-> [];
     _->duplication_situation_detected
end.
