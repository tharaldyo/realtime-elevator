-module(order_manager).
-compile(export_all).

-record (order, {floor, direction}).

start() ->
	register(order_manager, self()),
	io:format("order_manager registered.~n"),

	mnesia:create_schema([node()]), % use [node()|nodes()] to install on all nodes
    rpc:multicall(node(), application, start, [mnesia]),
	mnesia:create_table(order, [{attributes, record_info(fields, order)},
                                {index, [#order.floor]},
								{disc_copies, [node()]}]),
                                {type, set}]),
	io:format("Mnesia initialized~n"),

	Test = #order{floor=2, direction=down},
	% Mnesia stuff largely incomplete
	NewOrderDirection=Test#order.direction,
	io:format("NewOrderDirection: ~p~n", [NewOrderDirection]),
    
    
    mnesia:write(NewOrderDirection),
    Ret = mnesia:read({order, 2}),

    io:format("Test: ~p~n", [Ret]).


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
