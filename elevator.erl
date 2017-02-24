-module(elevator).
-export([start/0]).

start() ->
	connection_manager:start(),
	%IdlePID = spawn(fun idle/0),
	IdlePID = spawn(fun order_manager:temp_recv/0),
	OrderManagerPID = spawn(fun order_manager:start/0),
	elev_driver:start(IdlePID, elevator),
	io:format("Elevator pid: ~p~n", [self()]),
	%io:format("hello from below spawn~n"). %debug
	IdlePID ! {floor_reached, 2}. %debug


idle() ->
	receive
		{floor_reached, 0} ->
			elev_driver:set_motor_direction(up),
			io:format("~p~n", [{floor_reached, 0}]); %debug
		{floor_reached, 2} ->
			elev_driver:set_motor_direction(down),
			io:format("~p~n", [{floor_reached, 2}]); %debug
		_Message ->
			io:format("~p~n", [_Message])

		end,


	idle().
