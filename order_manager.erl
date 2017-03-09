-module(order_manager).
-export([start/0, add_order/2, remove_order/2]).
-record(order, {floor, direction}).

start() ->
	order_queue_init(global_order_table, orderman),
	order_queue_init(local_order_table, localorderman).

add_order(Floor, Direction) ->
	case Direction of
		command ->
			localorderman ! {add_order, #order{floor = Floor, direction = Direction}};
		_Else ->
			orderman ! {add_order, #order{floor = Floor, direction = Direction}}
			%lists:foreach(fun(Node) -> {orderman, Node} ! {add_order, #order{floor = Floor, direction = Direction}} end, nodes())
			end.

remove_order(QueueName, Order) ->
	QueueName ! {remove_order, Order},
	case QueueName of orderman ->
		lists:foreach(fun(Node) -> {orderman, Node} ! {remove_order, Order} end, nodes())
	end.

order_queue_init(FileName, QueueName) ->
	io:format("Loading ~p order table.~n", [QueueName]),
	dets:open_file(FileName, [{type, bag}]),
	OrdersFromDisk = dets:lookup(FileName, order),
	dets:close(FileName),
	register(QueueName, spawn(fun() -> order_queue(OrdersFromDisk) end)).

order_queue(Orders) ->
	io:format("Orderlist: ~p~n", [Orders]),
	receive
		{add_order, NewOrder} ->
			io:format("Adding new order.~n"),
			case sets:is_element(NewOrder, sets:from_list(Orders)) of
				false ->
					dets:open_file(order_table, [{type, bag}]),
					%io:format("Syntax: ~p~n", [NewOrder]), %debug
					dets:insert(order_table, NewOrder),
					dets:close(order_table),
					order_queue(Orders ++ [NewOrder]);
				true ->
					io:format("Order already exists.~n"), %debug
					order_queue(Orders)
			end;

		{remove_order, Order} ->
			dets:open_file(order_table, [{type, bag}]),
			dets:delete_object(order_table, Order),
			dets:close(order_table),
			order_queue(Orders--[Order]);

		{get_orders, PID} ->
			io:format("get_orders received ~n"),
			PID ! {orders, Orders},
			order_queue(Orders)
		end.

	% HOW TO HANDLE NETWORK SPLITS
	% Broadcast regularly all orders, ref function below.
	% Could do this every time order added in order_queue/1.
	% Much network traffic? Not really. At most 6 items per node.

	% orderman ! {get_orders, self()}
	% receive...
	% for all nodes():
	% for all orders:
	% {orderman, NOde} ! {add_order, order};
	%

	% for add_order
%broadcoast_orders() ->
%	Orders =
%	lists:foreach()
