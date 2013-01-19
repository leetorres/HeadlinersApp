%%%-------------------------------------------------------------------
%%% @author Robin, Lee, Chayi, David, Chris, Ashley
%%% @copyright (C) 2012, Headliners
%%% @doc
%%% This module is responsible for the flow of data to and from the how it works box
%%% @end
%%%-------------------------------------------------------------------

-module(headliners_app_how_it_works_controller, [Req]).
-compile(export_all).

-define(NewSessionId, "Searching").

% @doc
%Function responsible for redirecting a user to the favorites page
redirect('GET',[]) ->
    {action_other, [{action, "index"}]}.

% @doc
%Function returns that to the index, letting it know if a user is logged
%in
index('GET',[])->
    LoginStatus = sessions:login_status(),
    case LoginStatus of
	undefined ->
	    {ok,[{uniqueID, LoginStatus}]};
	_Active ->
	    ID = sessions:get_unique_id(),
	    {ok, [{uniqueID, ID}]}
    end.
    
