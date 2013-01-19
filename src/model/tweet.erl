%%%-------------------------------------------------------------------
%%% @author Robin and Lee
%%% @copyright (C) 2012, Headliners
%%% @doc
%%% This module provides a database schema for chicagoboss.
%%% @end
%%%-------------------------------------------------------------------

-module(tweet, [Id, CreatedAt, FromUser,FromuserIdstr, FromuserName,
	       IdStr, IsolanguageCode, ProfileimageUrl, ProfileimageUrlhttps,
	       Source, Text, TouserIdstr, Geo, Likes
]).
-compile(export_all).

% @doc
% Business Intelligence
% This function will make a twitter query to get information about the user 
% that created the tweet that is being put into the database
after_create()->
    UserId = fromuser_idstr(),
    spawn(fun() -> intwitter_query:get_intwitter(UserId) end).
