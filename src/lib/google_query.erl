%%% @author Chayi "Bear" Fan, Robin Murén
%%% @copyright (C) 2012, Bear
%%% @doc 
%%% This module is used to handle google queries. 
%%% @end

-module(google_query).
-compile(export_all).
-include_lib("xmerl/include/xmerl.hrl").

% Main function that will take a 'KeyWord', start ssl and inets, and make a httpc request to
% google, requesting search results for the 'KeyWord' and expect a return in atom format.
get_google(KeyWord) ->
    Key=re:replace(KeyWord, " ", "+", [global, {return, list}]),
    ssl:start(),
    inets:start(),
    
    % Body is the results    
    case httpc:request(get, {build_google_url(Key),[]},[{timeout, 6000}],[]) of 
	{ok, {_,_,Body}} -> parse_google(Body);
	Other -> {error, Other} 
    end.

% Builds a restful url that can be used to query google. Quite simply just concatinates the 'KeyWord' to the end of a static url.
% A few parameters has been
build_google_url(KeyWord) -> 
    "https://www.googleapis.com/customsearch/v1?key=AIzaSyDIO6i_qk2W50cORQeIVvJ_pA9mVjMuZew&cx=013036536707430787589:_pqjad5hr1a&alt=atom&q=" ++ KeyWord.

% @doc
% Parsing the data into records.
parse_google(Body)->
    {Xml,_} = xmerl_scan:string(Body),
    Elements = Xml#xmlElement.content,
    getresult(Elements,[],[],[]).

% @doc
% Getting the results
getresult([#xmlElement{name=entry,content=Content}|Rest],Ids,Titles,Summarys)->
    Id=lists:append(getid(Content)),
    Title=lists:append(gettitle(Content)),
    Summary=lists:append(getsummary(Content)),
    getresult(Rest,[Id|Ids],[Title|Titles],[Summary|Summarys]);
getresult([_ | Rest],Ids,Titles,Summarys) ->
    getresult(Rest,Ids,Titles,Summarys);
getresult([],Ids,Titles,Summarys) ->
    get_all(Ids,Titles,Summarys).
	
% @doc
% retrieving title from the records
gettitle([#xmlElement{name=title,content=Content}|Rest])->
    Title=combinetitle(Content),
    gettitle(Rest),
    Title;    
gettitle([_ | Rest]) ->
    gettitle(Rest);
gettitle([]) ->
     [].

% @doc
% retrieving id from the records
getid([#xmlElement{name=id,content=Ids}|Rest])->
    Id=combinetitle(Ids),
    getid(Rest),
    Id;
getid([_ | Rest]) ->
    getid(Rest);
getid([]) ->
     [].

% @doc
% retrieving summary from the records
getsummary([#xmlElement{name=summary,content=Content}|Rest])->
    Summary=combinetitle(Content),
    getsummary(Rest),
    Summary;
getsummary([_ | Rest]) ->
    getsummary(Rest);
getsummary([]) ->
     [].

combinetitle([#xmlText{value=Value}|Rest])->
    [Value | combinetitle(Rest)];
combinetitle([]) ->
    [].

% @doc
% taking all the results and storing it in the database.
get_all([],[],[]) ->
    {ok, got_results};
get_all([H|T],[H1|T1],[H2|T2])->
    case boss_db:count(googles,[{link,'matches',H}]) of
    0->
	    Google=google:new(id,check_str(H1),check_str(H),check_str(H2)),
	    Google:save(),
	    get_all(T,T1,T2);
	_Other ->
            get_all(T,T1,T2)
    end.

% @doc
% Takes a string and checks if it contains any ascii values above 255, 
% if it does it will replace it with the ascii value of a blank space (32)
check_str([]) ->
    [];
check_str(List) ->
    check_str(List, []).
check_str([H|T], NewList) when H =< 255 ->
    check_str(T, NewList ++ [H]);
check_str([H|T], NewList) when H > 255 ->
    check_str(T, NewList ++ [32]);
check_str([], NewList) ->
    NewList.
