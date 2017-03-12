-module(state_machine).
-export([start/0]).
-include("constants.hrl").

start() ->
  state_initializing().

state_initializing() ->
  %io:format("state_machine initializing~n"),
  elevatorman ! {fsm, intializing},
  receive floor_reached ->
    elevatorman ! {fsm, initialized}
  end,

  state_idle().

state_idle() ->
  io:format("~nSTATE MACHINE: elevator says: hello, I'm idle! ~n"),
  elevatorman ! idle,
  receive
    {drive, Direction} ->
      io:format("STATE MACHINE: I received a command to start driving, I will start driving now ~n"),
      %driverman ! {set_motor, Direction},
      %stateman ! {update_state, direction, Direction},
      state_driving();
    floor_reached ->
      state_doors_open()

    after 1000 ->
      io:format("STATE MACHINE: state_idle just timed out, calling again =) ~n"),
      state_idle()
  end.

state_driving() ->
  io:format("STATE MACHINE: driving ~n"),
  receive
    floor_reached ->
      io:format("STATE MACHINE: received floor_reached, stopping, opening doors~n"),
      driverman ! {set_motor, stop},
      state_doors_open();
    floor_passed ->
      state_driving()

    after 10000 ->
      state_lost()
    end.

state_doors_open() ->
  io:format("STATE MACHINE: opening doors~n"),
  driverman ! open_door,
  timer:sleep(?DOOR_OPEN_TIME),
  io:format("STATE MACHINE: closing doors~n"),
  driverman ! close_door,
  state_idle().
  %io:format("hello from doors_open ~n").

state_lost() ->
  stateman ! {update_state, state, lost}, % TODO: decide whether to send this to elevatorman or stateman
  io:format("~s,~n", [color:red("STATE MACHINE: ELEVATOR IS LOST!")]),

  receive
    floor_reached ->
      driverman ! {set_motor, stop},
      state_doors_open();
    floor_passed ->
      driverman ! {set_motor, stop},
      state_idle()
  end.
