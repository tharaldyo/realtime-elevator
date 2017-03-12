-module(order_manager).
-record(order, {floor, direction}).
-include("constants.hrl").

-export([	start/0,
 					add_order/2,
					remove_order/2,
					get_orders/1 ]).

start() ->
	order_queue_init(global_order_table, orderman),
	order_queue_init(local_order_table, localorderman).

add_order(Floor, Direction) ->
	case Direction of
		command ->
			localorderman ! {add_order, #order{floor = Floor, direction = Direction}};
		_Other ->
			orderman ! {add_order, #order{floor = Floor, direction = Direction}},
      broadcast_orders()
	end.

remove_order(QueueName, Order) ->
	io:format("ORDER MANAGER: remove_order(~p, ~p)~n", [QueueName, Order]),
	{_,Floor,Direction} = Order,
	QueueName ! {remove_order, #order{floor = Floor, direction = Direction}},
  case QueueName of
    orderman ->
      lists:foreach(fun(Node) -> {orderman, Node} ! {remove_order, Order} end, nodes());
    _ -> ok
  end.

get_orders(QueueName) ->
	QueueName ! get_orders,
	receive
		Orders ->
			Orders
    after ?RECEIVE_BLOCK_TIME ->
      io:format("~s Order manager waiting for orders in get_orders().~n", [color:red("RECEIVE TIMEOUT:")])
	end.

order_queue_init(FileName, QueueName) ->
	io:format("ORDER MANAGER: Loading ~p order table.~n", [QueueName]),
	dets:open_file(FileName, [{type, bag}]),
	OrdersFromDisk = dets:lookup(FileName, order),
	dets:close(FileName),
	register(QueueName, spawn(fun() -> order_queue(OrdersFromDisk, FileName) end)).

order_queue(Orders, FileName) ->
	io:format("ORDER MANAGER: Orderlist ~p changed: ~p~n", [FileName, Orders]),
	receive
		{add_order, NewOrder} ->
			case sets:is_element(NewOrder, sets:from_list(Orders)) of
				false ->
					dets:open_file(FileName, [{type, bag}]),
					dets:insert(FileName, NewOrder),
					dets:close(FileName),
          % TODO: review line below...
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
			PID ! {orders, Orders},
			order_queue(Orders, FileName)
	end.

broadcast_orders() ->
  io:format("broadcast broadcast!~n"),
  orderman ! {get_orders, self()},
  GlobalOrders =
  receive
    {orders, Orders} ->
      Orders
    after ?RECEIVE_BLOCK_TIME ->
      io:format("~s Order manager didn't get orders to broadcast.~n", [color:red("RECEIVE TIMEOUT:")]) end,

  lists:foreach(fun(Node) ->
    lists:foreach(fun(Order) -> {orderman, Node} ! {add_order, Order} end, GlobalOrders)
  end, nodes()).
