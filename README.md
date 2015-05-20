gen_cycle
=========

![Build Status](https://travis-ci.org/aerosol/gen_cycle.svg)

`gen_cycle` is a simple behaviour for recurring (on a regular basis) supervised tasks.
Every task (cycle) is a separate process, calling arbitrary callback every N milliseconds.
The callback results with `cycle_op()` type being a directive to either continue, continue hibernated, or stop.
Additionally, a cycle can handle arbitrary Erlang messages. The result of message handling is expected to be `cycle_op()` as well.

test
====

```
$ rebar compile ct
==> gen_cycle (compile)
==> gen_cycle (ct)
DONE.
Testing dev.gen_cycle: TEST COMPLETE, 5 ok, 0 failed of 5 test cases
```

example
=======

A sample recurring task is implemented in tests. See [here](https://github.com/aerosol/gen_cycle/blob/develop/test/sample_cycle.erl).

callbacks
=========

```erlang
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
```
