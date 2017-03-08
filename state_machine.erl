-module(state_machine).
-export([start/0]).

start() ->
  spawn(fun state_initializing/0).

state_initializing() ->
  elevatorman ! {fsm, intializing},
  receive {floor_reached} ->
    elevatorman ! {fsm, initialized}
  end,

  state_idle().

state_idle() ->
  elevatorman ! idle,
  receive
    {move} -> %{move, Direction} ->
      state_driving();
    {floor_reached} ->
      state_doors_open()
    end.

state_driving() ->
  io:format("hello from driving ~n"),
  receive
    {floor_reached} ->
      elevatorman ! {set_motor, stop},
      state_doors_open();
    {floor_passed} ->
      state_driving();
    {endpoint} ->
      driverman ! {set_motor, stop},
      state_idle()

    after 5000 ->
      state_lost()
    end.



state_doors_open() ->
  io:format("hello from doors_open ~n").

state_lost() ->
  elevatorman ! lost,
  receive
    {floor_reached} ->
      elevatorman ! {set_motor, stop},
      state_idle();
    {floor_passed} ->
      elevatorman ! {set_motor, stop},
      state_idle();
    {endpoint} ->
      elevatorman ! {set_motor, stop},
      state_idle()
  end.
