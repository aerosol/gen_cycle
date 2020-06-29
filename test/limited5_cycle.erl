-module(limited5_cycle).
-behaviour(gen_cycle).

-export([start/1, start/2,
         init_cycle/1,
         handle_cycle/1,
         handle_info/2]).

start(Args) ->
    gen_cycle:start_supervised(?MODULE, Args).

start(Name, Args) ->
    gen_cycle:start_supervised(Name, ?MODULE, Args).

init_cycle([TestProcess, Interval]) ->
    {ok, {Interval, {TestProcess, 0, 0}}}.

handle_cycle({TestProcess, 5 = CycleCount, InfoCount}) ->
    {stop, {TestProcess, CycleCount, InfoCount}};
handle_cycle({TestProcess, CycleCount, InfoCount}) ->
    TestProcess ! cycle_handled,
    {continue, {TestProcess, CycleCount + 1, InfoCount}}.

handle_info(get_cycle_count, {TestProcess, CycleCount, _InfoCount} = State) ->
    TestProcess ! {get_cycle_count, CycleCount},
    {continue, State};
handle_info(get_info_count, {TestProcess, _CycleCount, InfoCount} = State) ->
    TestProcess ! {get_info_count, InfoCount},
    {continue, State};
handle_info(Msg, {TestProcess, CycleCount, InfoCount}) ->
    TestProcess ! {info_handled, Msg},
    {continue, {TestProcess, CycleCount, InfoCount + 1}}.
