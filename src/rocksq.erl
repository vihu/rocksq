%%%-------------------------------------------------------------------
%% @doc rocksq public API
%% @end
%%%-------------------------------------------------------------------

-module(rocksq).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    rocksq_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
