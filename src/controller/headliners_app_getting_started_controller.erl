%%%-------------------------------------------------------------------
%%% @author Robin, Lee, Chayi, David, Chris, Ashley
%%% @copyright (C) 2012, Headliners
%%% @doc
%%% This module is responsible for the flow of data to and from the registration page
%%% @end
%%%-------------------------------------------------------------------

-module(headliners_app_getting_started_controller, [Req]).
-compile(export_all).

-define(NewSessionId, "Searching").

% @doc
%Function responsible for redirecting a user to the favorites page
redirect('GET',[]) ->
    {action_other, [{action, "index"}]}.

% @doc
%Function that provides the data for the user's favorite tweets and
%the business intelligence for the company's favorites
index('GET',[])->
    LoginStatus = sessions:login_status(),
    case LoginStatus of
	undefined ->
	    {ok,[{loginStatus, LoginStatus}]};
	_Active ->
	    {ok, [{loginStatus, LoginStatus}]}
    end.

