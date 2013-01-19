%%%-------------------------------------------------------------------
%%% @author Christopher Comella
%%% @copyright (C) 2012, Headliners
%%% @doc
%%% THIS MODULE IS NOT IN USE AT THE MOMENT!!
%%% @end
%%%-------------------------------------------------------------------

-module(login, [Req,SessionID]).
-compile(export_all).

-define(NewSessionId, "Searching").

start_session() ->
%    boss_session:new_session(?NewSessionId),
    Username = case boss_session:get_session_data(?NewSessionId, "username") of
	undefined ->
			"test";
	Email ->
			Email
	       end,
    Username.

