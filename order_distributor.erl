-module(order_distributor).
-include("constants.hrl").
-export([start/0]).
-record(order, {floor, direction}).

start() ->
  io:format("ORDER DISTRIBUTOR: order_distributor started ~n"),
  distributor().

distributor() ->
  receive
    get_order ->
      localorderman ! {get_orders, self()},
      receive
        {orders, []} ->
          %io:format("no local orders available, checking global orders ~n"),

          orderman ! {get_orders, self()},
          receive
            {orders, []} ->
              %io:format("no orders available, sending empty list ~n"),
              elevatorman ! {order, []};

            {orders, GlobalOrderList} -> % get first order in queue (FIFO)
              [GlobalOrder|_Disregard] = GlobalOrderList,
              %io:format("order received: ~p~n", [GlobalOrder]), %debug
              {Executor, Others} = get_best_elevator(GlobalOrder),
              %io:format("The executor: ~p~n", [list_to_atom(element(1, Executor))]), %debug
              %io:format("will receive this floor: ~p~n", [Order#order.floor]),

              case Executor of
                [] ->
                  io:format("ORDER DISTRIBUTOR: No executor found :-( ~n");
                Executor ->
                  case element(2, Executor) of
                    idle ->
                      {elevatorman, list_to_atom(element(1, Executor))} ! {order, GlobalOrder},
                      %io:format("order_distributor removing~n"),
                      order_manager:remove_order(orderman, GlobalOrder);

                    busy ->
                      io:format("ORDER DISTRIBUTOR: A driving elevator will complete the order: ~p~n", [Executor])
                  end,

                % send empty lists to all elevator who lost
                  lists:foreach(fun(LosingElevator) ->
                    {elevatorman, list_to_atom(element(1, LosingElevator))} ! {order, []}
                    end, Others)
              end
          after ?RECEIVE_BLOCK_TIME -> io:format("~s Order distributor waiting for GLOBAL orders.~n", [color:red("RECEIVE TIMEOUT:")])
          end;

        {orders, LocalOrderList} ->
          [LocalOrder|_D] = LocalOrderList,
          %io:format("local order received: ~p~n", [LocalOrder]), %debug
          elevatorman ! {order, LocalOrder}
          %io:format("order_distributor removing~n"),
          %order_manager:remove_order(localorderman, LocalOrder)
      after ?RECEIVE_BLOCK_TIME -> io:format("~s Order distributor waiting for LOCAL orders.~n", [color:red("RECEIVE TIMEOUT:")])
      end
  end,
  distributor().

get_best_elevator(Order) ->
  Elevators = get_all_elevators(Order),
  io:format("ORDER DISTRIBUTOR: Elevators: ~p~n", [Elevators]), %debug
  % TODO: there is a bug here which causes the pattern match below to fail
  CostList = lists:map(fun(Elevator) -> {abs(element(3, Elevator) - Order#order.floor), Elevator} end, Elevators),
  io:format("ORDER DISTRIBUTOR: Costlist looks like: ~p~n", [CostList]),
  case CostList of
    [] ->
      Executor = [];
      %io:format("tried to find best elevator, but no elevators are available at the moment ~n");

    CostList ->
      [{_Cost, Executor} | _Disregard] = lists:keysort(1, CostList)
  end,

  {Executor, Elevators--[Executor]}.

  % Executor = element(2, ExecutorTuple),
  % Order#order.floor

get_all_elevators(Order) ->
  % returns a list of all the elevators and their state: [elevator1, elevator2, ..]
  ListCreator = spawn(fun() -> elevator_list([]) end), % TODO: turn this into a map?
  lists:foreach(fun(Node) ->
    {stateman, Node} ! {get_state, self()},
    receive {elevator_state, Elevator} ->
      case element(2, Elevator) of
        idle ->
          ListCreator ! {add_elevator, Elevator};
        busy -> % moving elevators  can also take an order, if..
          if
            Order#order.direction == element(4, Elevator) -> % check if order direction equals elevator direction
              CurrentFloor = element(3, Elevator),
              TargetFloor = (element(5, Elevator))#order.floor,

              if
                CurrentFloor < TargetFloor ->
                  case lists:member(Order#order.floor, lists:seq(CurrentFloor, TargetFloor)) of
                     true ->
                      ListCreator ! {add_elevator, Elevator};
                     false -> ok
                  end;
                CurrentFloor > TargetFloor ->
                  case lists:member(Order#order.floor, lists:seq(TargetFloor, CurrentFloor)) of
                     true ->
                      ListCreator ! {add_elevator, Elevator};
                     false -> ok
                  end;
                true ->
                  io:format("ORDER DISTRIBUTOR: elevator is done with its current order ~n"),
                  ListCreator ! {add_elevator, Elevator}
              end;
            true -> ok
          end; %debug: ends the first if

        _ ->
          io:format("ORDER DISTRIBUTOR: elevator not idle or driving: ~p~n", [Elevator])
      end %debug: ends the first case
    after 500 ->
      io:format("~sORDER DISTRIBUTOR: failed to get state from node: ~p, WILL DISCONNECT IT~n", [color:red("RECEIVE TIMEOUT:"),Node]),
      erlang:disconnect_node(Node)
    end
  end, [node()|nodes()]), %debug: ends the foreach

    ListCreator ! return_list,
    %io:format("hello from I just asked for list ~n"), %debug
    Elevators = receive {elevator_list, ElevatorList} -> ElevatorList
    after ?RECEIVE_BLOCK_TIME ->
      io:format("~s Order distributor waiting to get elevator_list.~n", [color:red("RECEIVE TIMEOUT:")]),
      []
    end,

    Elevators.

elevator_list(Elevators) ->
  receive
    {add_elevator, Elevator} ->
      elevator_list(Elevators++[Elevator]);
    return_list ->
      distributor ! {elevator_list, Elevators}
    after ?RECEIVE_BLOCK_TIME -> io:format("~s Order distributor waiting to get elevator_list.~n", [color:red("RECEIVE TIMEOUT:")])
  end.

%get_states() ->
  %foreach(get_state, [node|nodes()]
