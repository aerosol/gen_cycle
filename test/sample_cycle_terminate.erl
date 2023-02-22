-module(sample_cycle_terminate).
-behaviour(gen_cycle).

-export([start/1, start/2,
         init_cycle/1,
         handle_cycle/1,
         handle_info/2,
         terminate/2]).

start(Args) ->
    gen_cycle:start_supervised(?MODULE, Args).

start(Name, Args) ->
    gen_cycle:start_supervised(Name, ?MODULE, Args).

init_cycle([TestProcess, Interval]) when is_pid(TestProcess) ->
    process_flag(trap_exit, true),
    {ok, {Interval, TestProcess}}.

handle_cycle(_TestProcess) ->
    {stop, boom}.

handle_info(_Msg, TestProcess) ->
    {continue, TestProcess}.

terminate(Reason, TestProcess) -> 
    io:format("~p", [{Reason, TestProcess}]),
    TestProcess ! {terminated, Reason},
    ok.
