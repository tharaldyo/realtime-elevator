-module(state_machine).
-export([start/0]).
-record(order, {floor, direction}).

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
  io:format("~nelevator says: hello, I'm idle! ~n"),
  elevatorman ! idle,
  receive
    {drive, Direction} ->
      io:format("I received a command to start driving, I will start driving now ~n"),
      driverman ! {set_motor, Direction},
      stateman ! {update_state, direction, Direction},
      state_driving();
    floor_reached ->
      state_doors_open()

    after 1000 ->
      io:format("state_idle just timed out, calling again =) ~n"),
      state_idle()
  end.

state_driving() ->
  io:format("DRIVING ~n"),
  receive
    floor_reached ->
      driverman ! {set_motor, stop},
      state_doors_open();
    floor_passed ->
      state_driving();
    endpoint ->
      driverman ! {set_motor, stop},
      state_idle()

    after 10000 ->
      state_lost()
    end.

state_doors_open() ->
  driverman ! open_door,
  timer:sleep(2000),
  elevatorman ! close_door,
  state_idle().
  %io:format("hello from doors_open ~n").

state_lost() ->
  elevatorman ! lost,
  io:format("~s,~n", [color:red("ELEVATOR IS LOST!")]),

  receive
    floor_reached ->
      elevatorman ! {set_motor, stop},
      state_idle();
    floor_passed ->
      elevatorman ! {set_motor, stop},
      state_idle()
  end.
