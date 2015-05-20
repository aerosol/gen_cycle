-module(gen_cycle_SUITE).
-compile([export_all]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [t_aborted_cycle,
     t_cycle_handled,
     t_info_handled,
     t_callback_stop,
     t_callback_hibernated
     ].

suite() ->
    [{ct_hooks,[cth_surefire]}, {timetrap, {seconds, 30}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    gen_cycle_app:start(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    gen_cycle_app:stop(),
    ok.

t_aborted_cycle(_Config) ->
    {ok, undefined} = sample_cycle:start(abort).

t_cycle_handled(_Config) ->
    {ok, Pid} = sample_cycle:start([self(), timer:seconds(1)]),
    receive
        cycle_handled ->
            receive
                cycle_handled ->
                    ok
            after timer:seconds(3) ->
                      error(second_cycle_handle_failure)
            end
    after timer:seconds(3) ->
              error(first_cycle_handle_failure)
    end,
    true = erlang:is_process_alive(Pid).

t_info_handled(_Config) ->
    {ok, Pid} = sample_cycle:start([self(), timer:seconds(1)]),
    Pid ! i_need_this_back,
    receive
        cycle_handled ->
            receive
                {info_handled, i_need_this_back} ->
                    ok
            after timer:seconds(3) ->
                      error(info_handle_failure)
            end
    after timer:seconds(3) ->
              error(first_cycle_handle_failure)
    end,
    true = erlang:is_process_alive(Pid).

t_callback_stop(_Config) ->
    Reply = {stop, normal},
    {ok, Pid} = sample_cycle:start([self(), timer:seconds(1), Reply]),
    true = erlang:is_process_alive(Pid),
    receive
        cycle_handled ->
            receive
                Any ->
                    error(Any)
            after timer:seconds(3) ->
                      ok
            end
    after timer:seconds(3) ->
              ok
    end,
    false = erlang:is_process_alive(Pid).

t_callback_hibernated(_Config) ->
    Reply = {continue_hibernated, self()},
    {ok, Pid} = sample_cycle:start([self(), timer:seconds(10), Reply]),
    true = erlang:is_process_alive(Pid),
    receive
        cycle_handled ->
            ok
    after timer:seconds(3) ->
              error(first_cycle_handle_failure)
    end,
    true = erlang:is_process_alive(Pid).
