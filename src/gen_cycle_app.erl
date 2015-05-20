-module(gen_cycle_app).
-behaviour(application).

-export([start/0,
         start/2,
         stop/0,
         stop/1]).

start() ->
    application:ensure_all_started(gen_cycle).

start(_StartType, _StartArgs) ->
    gen_cycle_sup:start_link().

stop() ->
    application:stop(gen_cycle).

stop(_State) ->
    ok.
