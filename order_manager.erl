-module(order_manager).
-record(order, {floor, direction}).
-export([	start/0,
 					add_order/2,
					remove_order/2,
					get_orders/1 ]).

%% TODO:
%% As it stand the queues are spawned, but the whole queue system is wrapped in
%% a "synchronous" layer... Interafce should be 100% message based, so will have
%% to add records elsewhere as well.

start() ->
	order_queue_init(global_order_table, orderman),
	order_queue_init(local_order_table, localorderman).

add_order(Floor, Direction) ->
	case Direction of
		command ->
			localorderman ! {add_order, #order{floor = Floor, direction = Direction}};
		OtherDirection ->
			orderman ! {add_order, #order{floor = Floor, direction = Direction}},
      broadcast_orders()
	end.

remove_order(QueueName, Order) ->
	io:format("remove_order(~p, ~p)~n", [QueueName, Order]),
	{_,Floor,Direction} = Order,
	%io:format("Floor: ~p, Direction: ~p~n", [Floor, Direction]),
	QueueName ! {remove_order, #order{floor = Floor, direction = Direction}}.

get_orders(QueueName) ->
	QueueName ! get_orders,
	receive
		Orders ->
			Orders
	end.

order_queue_init(FileName, QueueName) ->
	io:format("Loading ~p order table.~n", [QueueName]),
	dets:open_file(FileName, [{type, bag}]),
	OrdersFromDisk = dets:lookup(FileName, order),
	dets:close(FileName),
	register(QueueName, spawn(fun() -> order_queue(OrdersFromDisk, FileName) end)).

order_queue(Orders, FileName) ->
	io:format("Orderlist of ~p: ~p~n", [FileName, Orders]),
	receive
		{add_order, NewOrder} ->
			case sets:is_element(NewOrder, sets:from_list(Orders)) of
				false ->
					dets:open_file(FileName, [{type, bag}]),
					dets:insert(FileName, NewOrder),
					dets:close(FileName),
          elev_driver:set_button_lamp(element(2, NewOrder),element(3, NewOrder), on),
					order_queue(Orders ++ [NewOrder], FileName);
				true ->
					order_queue(Orders, FileName)
			end;

		{remove_order, Order} ->
			dets:open_file(FileName, [{type, bag}]),
			dets:delete_object(FileName, Order),
			dets:close(FileName),
			order_queue(Orders--[Order], FileName);

		{get_orders, PID} ->
			%io:format("get_orders received ~n"),
			PID ! {orders, Orders},
			order_queue(Orders, FileName)
		end.

% this function should be used to remove
broadcast_orders() ->
  orderman ! {get_orders, self()},
  GlobalOrders = receive {orders, Orders} -> Orders end,

  lists:foreach(fun(Node) ->
    lists:foreach(fun(Order) -> {orderman, Node} ! {add_order, Order} end, Orders)
  end, nodes()).
