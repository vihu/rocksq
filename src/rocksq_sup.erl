%%%-------------------------------------------------------------------
%% @doc rocksq top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(rocksq_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(WORKER(I, Args), #{
                    id => I,
                    start => {I, start_link, Args},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [I]
                   }).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},

    BaseDir = application:get_env(rocksq, base_dir, "data"),

    RocksQOpts = [{base_dir, BaseDir}],

    ChildSpecs = [
                  ?WORKER(rocksq_worker, [RocksQOpts])
                 ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
