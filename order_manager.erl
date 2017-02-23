-module(order_manager).
-export([start/0]).

start() ->
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


	start().
