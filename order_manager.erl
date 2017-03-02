-module(order_manager).
-export([start/0]).
-record (order, {floor, direction}).

start() ->
	register(orderman, self()),
	io:format("order_manager: registered, now spawning loop.~n"),
    spawn(fun loop/0).

loop() ->
  receive
    {new_order, Direction, Floor} ->
      % distribute new order
      io:format("order_manager: new order: ~p~p~n", [Direction, Floor]);
    {floor_reached, Floor} ->
      % update FSM?
      io:format("order_manager: floor reached: ~p~n", [Floor]);
    _Message ->
      io:format("order_manager: received invalid message: ~p~n",
                [_Message])
  end,
  loop().
