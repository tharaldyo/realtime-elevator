-module(order_manager).
-export([start/0, temp_recv/0]).
-record (order, {floor, direction}). % is this right?

start() ->
	register(orderman, self()),
	io:format("Orderman online and ready~n"),
	ok = mnesia:create_schema([node()]), % create schema on this node only, use [node()|nodes()] to install on all nodes
	application:start(mnesia),
	mnesia:create_table(order, [{attributes, record_info(fields, order)},
															{disc_copies, [node()]}]),
	io:format("Mnesia initialized~n"),
	%#order{floor=1, direction=up},
	Test = #order{floor=2, direction=down},

	% Mnesia stuff largely incomplete

	NewOrderDirection=Test#order.direction,
	io:format("NewOrderDirection: ~p~n", [NewOrderDirection]).




temp_recv() ->

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


	temp_recv().
