%%%-------------------------------------------------------------------
%%% @author Robin, Lee, Chayi, David, Chris, Ashley
%%% @copyright (C) 2012, Headliners
%%% @doc
%%% This module is responsible for the flow of data, to and from the about page.
%%% @end
%%%-------------------------------------------------------------------

-module(headliners_app_about_controller, [Req]).
-compile(export_all).

-define(NewSessionId, "Searching").

% @doc
% Function responsible for redirecting a user to the favorites page
redirect('GET',[]) ->
    {action_other, [{action, "index"}]}.

% @doc
%Function returns that to the index, letting it know if a user is logged
%in
index('GET',[])->
    LoginStatus = sessions:login_status(),
    case LoginStatus of
	undefined ->
	    {ok,[{loginStatus, LoginStatus}]};
	_Active ->
	    {ok, [{loginStatus, LoginStatus}]}
    end.

