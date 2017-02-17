-module(elevator).
-export([start/0]).

start() ->
	connection_manager:start(),
	IdlePID = spawn(fun idle/0),
	elev_driver:start(IdlePID, elevator),
	io:format("Elevator pid: ~p~n", [self()]),
	io:format("hello from below spawn~n"). %debug


idle() ->
	receive
		{floor_reached, 0} ->
			elev_driver:set_motor_direction(up),
			io:format("floor reached: 0 ~n"); %debug
		{floor_reached, 2} ->
			elev_driver:set_motor_direction(down),
			io:format("floor reached: 2 ~n"); %debug
		_Message ->
			io:format("~p~n", [_Message])

		end,


	idle().
