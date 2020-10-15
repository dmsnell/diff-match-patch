%%%-------------------------------------------------------------------
%% @doc dmp public API
%% @end
%%%-------------------------------------------------------------------

-module(dmp_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    dmp_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
