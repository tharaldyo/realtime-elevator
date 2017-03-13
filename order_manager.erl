-module(order_manager).
-record(order, {floor, direction}).
-include("constants.hrl").

-export([	start/0,
 					add_order/2,
					remove_order/2,
					get_orders/1 ]).

start() ->
	order_queue_init(global_order_table, orderman),
	order_queue_init(local_order_table, localorderman),
  spawn(fun order_synchronizer/0).

add_order(Floor, Direction) ->
  NewOrder = #order{floor = Floor, direction = Direction},
	case Direction of
		command ->
      LocalOrders = get_orders(localorderman),
      case sets:is_element(NewOrder, sets:from_list(LocalOrders)) of
				false ->
          localorderman ! {add_order, NewOrder};
        true ->
          ok
      end;
		_Other ->
      GlobalOrders = get_orders(orderman),
      case sets:is_element(NewOrder, sets:from_list(GlobalOrders)) of
				false ->
          orderman ! {add_order, NewOrder},
          broadcast_orders();
        true ->
          ok
      end
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
	QueueName ! {get_orders, self()},
	receive
		{orders, Orders} ->
			Orders
    after ?RECEIVE_BLOCK_TIME ->
      []
	end.

order_queue_init(FileName, QueueName) ->
	dets:open_file(FileName, [{type, bag}]),
	OrdersFromDisk = dets:lookup(FileName, order),
	dets:close(FileName),
  case QueueName of
    orderman -> broadcast_orders(OrdersFromDisk);
    localorderman -> ok end,
	register(QueueName, spawn(fun() -> order_queue(OrdersFromDisk, FileName) end)).

order_queue(Orders, FileName) ->
	receive
		{add_order, NewOrder} ->
			case sets:is_element(NewOrder, sets:from_list(Orders)) of
				false ->
					dets:open_file(FileName, [{type, bag}]),
					dets:insert(FileName, NewOrder),
					dets:close(FileName),
          driverman ! {set_hall_lamp, element(2, NewOrder), element(3, NewOrder), on),
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
  GlobalOrders = get_orders(orderman),

  lists:foreach(fun(Node) ->
    lists:foreach(fun(Order) -> {orderman, Node} ! {add_order, Order} end, GlobalOrders)
  end, nodes()).

broadcast_orders(OrderList) ->
  lists:foreach(fun(Node) ->
    lists:foreach(fun(Order) -> {orderman, Node} ! {add_order, Order} end, OrderList)
  end, nodes()).

order_synchronizer() ->
  timer:sleep(5000),
  broadcast_orders(),
  order_synchronizer().
