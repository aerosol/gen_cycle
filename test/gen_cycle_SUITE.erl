-module(gen_cycle_SUITE).
-compile([export_all]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [t_aborted_cycle,
     t_cycle_handled,
     t_info_handled,
     t_callback_stop,
     t_callback_hibernated,
     t_start_named_supervised,
     t_handle_info_does_not_trigger_new_cycle
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
    {current_function,
     {erlang,hibernate,3}} = process_info(Pid, current_function),
    true = erlang:is_process_alive(Pid).

t_start_named_supervised(_Config) ->
    Name = {local, named_cycle},
    {ok, _} = sample_cycle:start(Name, [timer:seconds(1)]),
    true = erlang:is_process_alive(whereis(named_cycle)).

t_handle_info_does_not_trigger_new_cycle(_Config) ->
    N = 50,
    {ok, Pid} = limited5_cycle:start([self(), timer:seconds(1)]),
    [begin
         Pid ! i_need_this_back
     end || _ <- lists:seq(1, N)],

    timer:sleep(2100),
    %% If handle_info() triggers new cycles, then it is very unlikely
    %% that Pid will be alive.  If scheduler luck is horrible, it is
    %% still alive, so we'll do more checking below.
    true = erlang:is_process_alive(Pid),

    Pid ! get_cycle_count,
    receive
        {get_cycle_count, 3} ->
            %% tick 1 = immediately after init(), tick 2 = after 1 sec,
            %% tick 3 = after 2 sec
            ok
    after timer:seconds(3) ->
            %% Pid is probably not alive.  If Pid is still alive, then
            %% the scheduler hates us.  Tell the test user what's in
            %% our mailbox to debug the problem.
            error({get_cycle_count_failure,
                   alive, erlang:is_process_alive(Pid),
                   mailbox, element(2, process_info(self(), messages))})
    end,

    Pid ! get_info_count,
    receive
        {get_info_count, N} ->
            ok;
        {get_info_count, Other2} ->
            error({get_info_count_failure, got, Other2})
    after timer:seconds(3) ->
            %% The scheduler really hates us.
            error({get_info_count_failure,
                   alive, erlang:is_process_alive(Pid),
                   mailbox, element(2, process_info(self(), messages))})
    end,
    ok.
