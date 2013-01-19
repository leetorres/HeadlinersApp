%%% @author Robin, Chayi, Lee, David, Ashley, Chris
%%% @copyright (C) 2012, Headliners
%%% @doc
%%%
%%% @end
-module(headliners_app_features_controller, [Req]).
-compile(export_all).

redirect('GET',[]) ->
    {action_other, [{action, "index"}]}.
