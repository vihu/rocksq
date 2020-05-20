-module(basic_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         all/0
        ]).

-export([
         basic_test/1
        ]).

%% common test callbacks

all() -> [
          basic_test
         ].

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

init_per_testcase(_TestCase, Config) ->
    {ok, Sup} = rocksq_sup:start_link(),
    [ {sup, Sup} | Config].

end_per_testcase(_TestCase, _Config) ->
    ok.

basic_test(_Config) ->
    true = rocksq_worker:is_empty(),
    ok = rocksq_worker:enqueue(<<"foo">>),
    false = rocksq_worker:is_empty(),
    ok = rocksq_worker:enqueue(<<"bar">>),
    false = rocksq_worker:is_empty(),
    ok = rocksq_worker:enqueue(<<"baz">>),
    false = rocksq_worker:is_empty(),
    {ok, <<"foo">>} = rocksq_worker:deque(),
    false = rocksq_worker:is_empty(),
    {ok, <<"bar">>} = rocksq_worker:deque(),
    false = rocksq_worker:is_empty(),
    {ok, <<"baz">>} = rocksq_worker:deque(),
    true = rocksq_worker:is_empty(),
    {error, empty_queue} = rocksq_worker:deque(),
    ok.
