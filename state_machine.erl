-module(state_machine).
-export([start/0]).

start() ->
  state_initializing().

state_initializing() ->
  io:format("state_machine initializing~n"),
  elevatorman ! {fsm, intializing},
  receive {floor_reached} ->
    elevatorman ! {fsm, initialized}
  end,

  state_idle().

state_idle() ->
  io:format("elevator says: hello, I'm idle! ~n"),
  elevatorman ! idle,
  receive
    {drive, Direction} -> %{move, Direction} ->
      driverman ! {set_motor, Direction},
      state_driving();
    floor_reached ->
      state_doors_open()

    after 5000 ->
        state_idle()
  end.

state_driving() ->
  %io:format("hello from driving ~n"),
  receive
    floor_reached ->
      driverman ! {set_motor, stop},
      state_doors_open();
    floor_passed ->
      state_driving();
    endpoint ->
      driverman ! {set_motor, stop},
      state_idle()

    after 5000 ->
      state_lost()
    end.

state_doors_open() ->
  elevatorman ! {doors, open},
  timer:sleep(2000),
  elevatorman ! {doors, close},
  state_idle().
  %io:format("hello from doors_open ~n").

state_lost() ->
  elevatorman ! lost,
  receive
    floor_reached ->
      elevatorman ! {set_motor, stop},
      state_idle();
    floor_passed ->
      elevatorman ! {set_motor, stop},
      state_idle();
    endpoint ->
      elevatorman ! {set_motor, stop},
      state_idle()
  end.
