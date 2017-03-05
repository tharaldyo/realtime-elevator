-module(elevator).
-export([start/0]).

start() ->
	order_manager:start(),
	MessageHandler = spawn(fun message_handler/0),
	elev_driver:start(MessageHandler, simulator),
	%connection_manager:start(),
	io:format("Elevator pid: ~p~n", [self()]).

% handle messages from the elevator driver
message_handler() ->
	receive
		{new_order, Floor, Direction} -> % TODO: do pattern matching for "command" somewhere
			io:format("New order: ~p~n", [Floor]),
		  order_manager:add_order(Floor, Direction);
		{floor_reached, 0} ->
			% elev_driver:set_motor_direction(up),
			io:format("~p~n", [{floor_reached, 0}]); %debug

		{floor_reached, 2} ->
			% elev_driver:set_motor_direction(down),
			io:format("~p~n", [{floor_reached, 2}]); %debug

		_Message ->
			io:format("~p~n", [_Message])
		end,
	message_handler().
