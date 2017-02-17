-module(elevator).
-export([start/0]).

start() ->
	IdlePID = spawn(fun idle/0),
	elev_driver:start(IdlePID, elevator),
	io:format("Elevator pid: ~p~n", [self()]),
	io:format("hello from below spawn").


idle() ->
	receive
		{floor_reached, 0} ->
			elev_driver:set_motor_direction(up),
			io:format("floor reached: 0 ~n");
		{floor_reached, 2} ->
			elev_driver:set_motor_direction(down),
			io:format("floor reached: 2 ~n");
		_Message ->
			io:format("~p~n", [_Message])

		end,


	idle().
