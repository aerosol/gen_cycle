-module(gen_cycle).
-behaviour(gen_server).

-type cycle_op() ::
    {continue, NewCycleData :: any()}
    | {continue_hibernated, NewCycleData :: any()}
    | {stop, Reason :: normal | any()}.

-export_type([cycle_op/0]).

-callback init_cycle([InitArgs :: any()]) ->
    {ok, {Interval :: pos_integer(),
          InitCycleData :: any()}}
    | ignore.

-callback handle_cycle(CycleData :: any()) ->
    cycle_op().

-callback handle_info(Msg :: any(), CycleData :: any()) ->
    cycle_op().

-export([start_supervised/2, start_supervised/3,
         start_link/2, start_link/3,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
          callback   :: module(),
          cycle_data :: term(),
          interval   :: pos_integer()
         }).

start_supervised(CallbackMod, Args) ->
    {ok, _} = gen_cycle_sup:start_child([CallbackMod, Args]).

start_supervised(Name, CallbackMod, Args) ->
    {ok, _} = gen_cycle_sup:start_child([Name, CallbackMod, Args]).

start_link(CallbackMod, InitArgs) ->
    gen_server:start_link(?MODULE, [CallbackMod, InitArgs], []).

start_link(Name, CallbackMod, InitArgs) ->
    gen_server:start_link(Name, ?MODULE, [CallbackMod, InitArgs], []).

init([CallbackMod, InitArgs]) ->
    case CallbackMod:init_cycle(InitArgs) of
        {ok, {Interval, CycleData}} ->
            run_cycle(0),
            {ok, #state{
                    callback = CallbackMod,
                    cycle_data = CycleData,
                    interval = Interval
                   }};
        ignore=I -> I
    end.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info('$cycle', State=#state{callback=C, interval=I, cycle_data=D}) ->
    V = C:handle_cycle(D),
    run_cycle(I),
    callback(V, State);
handle_info(Msg, State=#state{callback=C, cycle_data=D}) ->
    callback(C:handle_info(Msg, D), State).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

run_cycle(Interval) ->
    erlang:send_after(Interval, self(), '$cycle').

callback({continue, NewD}, State) ->
    {noreply, State#state{cycle_data=NewD}};
callback({continue_hibernated, NewD}, State) ->
    {noreply, State#state{cycle_data=NewD}, hibernate};
callback({stop, Reason}, State) ->
    {stop, Reason, State}.
