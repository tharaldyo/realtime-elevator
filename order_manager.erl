-module(order_manager).
-export([start/0, add_order/2]).
-record(order, {floor, direction}).

start() ->
	dets:open_file(order_table, [{type, bag}]),
	OrderTable = dets:lookup(order_table, order),
	dets:close(order_table),
	io:format("OrderTable: ~p~n", [OrderTable]),
	SomePID = spawn(fun() -> order_queue(OrderTable) end), %?MODULE, order_queue, [hello]),
	register(orderman, SomePID). % TODO: fix this

add_order(Floor, Direction) -> % TODO: look at this
	%orderman ! {add, {order, {Floor, Direction}}}.
	timer:sleep(200),
	add_order(#order{floor = Floor, direction = Direction}). % using record
	%add_order(Floor, Direction}). % using regular tuple

	%orderman ! {add, Floor, Direction}.

%add_order(Order) when Order#order.direction == command->
%	orderman ! {add, Order};

add_order(Order) ->
	io:format("order received, sending to orderman: ~p~n", [Order]),
	orderman ! {add_order, Order}.

% Note: ?MODULE metoden Sivert bruker kan også brukes her, men da MÅ
% order_queue/1 eksporteres!!
order_queue(Orders) ->
	io:format("Orderlist: ~p~n", [Orders]),

	receive
		{add_order, NewOrder} ->
			dets:open_file(order_table, [{type, bag}]),
			io:format("Syntax: ~p~n", [NewOrder]),
			dets:insert(order_table, NewOrder),

			dets:close(order_table),
			order_queue(Orders ++ [NewOrder]);

		{remove_order, Order} ->
			dets:open_file(order_table, [{type, bag}]),
			dets:delete_object(order_table, Order),
			dets:close(order_table),
			order_queue(Orders--[Order]);

		{get_orders, PID} ->
			io:format("get_orders received ~n"),
			PID ! Orders,
			order_queue(Orders)
			%?MODULE:order_queue(dets:to_list(dets:from_list(Orders)))

		end.
