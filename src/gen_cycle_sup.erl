-module(gen_cycle_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([start_child/1]).
-export([stop_child/1]).

-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, transient, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Args) ->
    supervisor:start_child(?MODULE, Args).

stop_child(Pid) when is_pid(Pid) ->
    supervisor:terminate_child(?MODULE, Pid).

init([]) ->
    {ok, {{simple_one_for_one, 500, 10},
          [ ?CHILD(gen_cycle, worker) ]}}.
