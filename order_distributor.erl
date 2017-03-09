-module(order_distributor).
-export([start/0]).
-record(order, {floor, direction}).

start() ->
  io:format("order_distributor started ~n"),
  distributor().

distributor() ->

  receive
    get_floor -> % main "gimme new order" call

      % check if any orders are available, else send [] to the elevator making the request
      orderman ! {get_orders, self()},
      receive
        {orders, []} ->
          io:format("no orders available, sending empty list ~n"),
          elevatorman ! {order, []};

        {orders, OrderList} -> % get first order in queue (FIFO)
          [Order|_Disregard] = OrderList,
          io:format("order received: ~p~n", [Order]), %debug
          % check if the order has direction "command" (comes from inside the elevator)
          case Order#order.direction of
            command ->
              % if yes, send order to "local" elevator
              io:format("received a command order, local elevator gets it ~n"), %debug
              elevatorman ! {order, Order#order.floor};

            _ ->
              Executor = find_best_elevator(Order),
              io:format("The executor: ~p~n", [Executor]) %debug
              % send order to executor


          end
        end

      % else, check states of elevators in Elevators
      % compare the floors of the idle elevators to the one in the order
      % assign the order by sending the floor number to the elevator who "wins"
      % ^ how? if an elevator is idle, it has sent a request for order to its own distributor, will this cause problems?
      % send something to the other elevators to let them know they "lost" (i.e. [])
      % this should all happen in distributor, though, this function only finds best elevator and returns it
      %send the order to Executor
      %send "no order for you" to Elevators--Executor
  end,
  distributor().

find_best_elevator(Order) ->
  Elevators = get_all_elevators(),
  io:format("Elevators: ~p~n", [Elevators]), %debug
  CostList = list:map(fun(Elevator) -> {abs(element(3, Elevator) - Order#order.floor), Elevator} end, Elevators),
  [{_Cost, Executor} | _Disregard] = lists:keysort(1, CostList),
  % Executor = element(2, ExecutorTuple),
  % Order#order.floor

  Executor. %debug: should return the winning elevator

get_all_elevators() ->
  % returns a list of all the elevators and their state: [elevator1, elevator2, ..]
  ListCreator = spawn(fun() -> elevator_list([]) end), % TODO: turn this into a map?
  lists:foreach(fun(Node) ->
    {stateman, Node} ! {get_state, self()},
    receive Elevator ->
      case element(2, Elevator) of
        idle ->
          ListCreator ! {add_elevator, Elevator};

        _ ->
          io:format("elevator not idle: ~p~n", [Elevator])
      end
    end
  end, [node()|nodes()]), %debug use [node()|nodes()]

    ListCreator ! return_list,
    io:format("hello from I just asked for list ~n"),
    Elevators = receive ElevatorList -> ElevatorList end,
    Elevators.

elevator_list(Elevators) ->
  receive
    {add_elevator, Elevator} ->
      elevator_list(Elevators++[Elevator]);
    return_list ->
      distributor ! Elevators
  end.

%get_states() ->
  %foreach(get_state, [node|nodes()]
