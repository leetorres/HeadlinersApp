%%% @author Robin <robin@robin-N53SV>
%%% @copyright (C) 2012, Robin
%%% @doc 
%%% This module is used to handle account creations. 
%%% @end
%%% Created : 11 Dec 2012 by Robin <robin@robin-N53SV>

-module(accounts).
-export([create_account/3]).

% @doc
% Create a new account with email, password and company
create_account(Email, Password, Company) ->
    NewUser = account:new(id, Email, Password, Company),
    NewUser:save().
