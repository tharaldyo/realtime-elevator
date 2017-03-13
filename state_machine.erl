-module(state_machine).
-include("constants.hrl").

-export([ start/0 ]).

start() ->
  state_initializing().

state_initializing() ->
  elevatorman ! {fsm, intializing},
  receive floor_reached ->
    elevatorman ! {fsm, initialized}
  end,
  state_idle().

state_idle() ->
  elevatorman ! idle,
  receive
    {drive, _Direction} ->
      state_driving();

    floor_reached ->
      state_doors_open();

    {state, driving} ->
      state_driving()

    after 1500 ->
      state_idle()
  end.

state_driving() ->
  receive
    floor_reached ->
      driverman ! {set_motor, stop},
      state_doors_open();
    floor_passed ->
      state_driving();
    {state, idle} ->
      state_idle()

    after 10000 ->
      state_lost()
    end.

state_doors_open() ->
  driverman ! open_door,
  timer:sleep(?DOOR_OPEN_TIME),
  driverman ! close_door,
  state_idle().

state_lost() ->
  stateman ! {update_state, state, lost},

  receive
    floor_reached ->
      driverman ! {set_motor, stop},
      state_doors_open();
    floor_passed ->
      driverman ! {set_motor, stop},
      state_idle();
    {state, idle} ->
      state_idle();
    {state, driving} ->
      state_driving()
  end,
  state_lost().
