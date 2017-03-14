-module(order_distributor).
-include("constants.hrl").
-record(order, {floor, direction}).

-export([ start/0 ]).

start() ->
  distributor().

distributor() ->
  receive
    get_order ->
      localorderman ! {get_orders, self()},
      receive
        {orders, []} ->
          orderman ! {get_orders, self()},
          receive
            {orders, []} ->
              elevatorman ! {order, []};

            {orders, GlobalOrderList} ->
              [GlobalOrder|_Disregard] = GlobalOrderList,
              {Executor, Others} = get_best_elevator(GlobalOrder),

              case Executor of
                [] ->
                  io:format("ORDER DISTRIBUTOR: No executor found :-( ~n");
                Executor ->
                  case element(2, Executor) of
                    idle ->
                      {elevatorman, list_to_atom(element(1, Executor))} ! {order, GlobalOrder},
                      order_manager:remove_order(orderman, GlobalOrder);
                    busy ->
                      io:format("ORDER DISTRIBUTOR: A driving elevator will complete the order: ~p~n", [Executor])
                  end,

                  lists:foreach(fun(LosingElevator) ->
                    {elevatorman, list_to_atom(element(1, LosingElevator))} ! {order, []}
                    end, Others)
              end
          after ?RECEIVE_BLOCK_TIME -> io:format("~s Order distributor waiting for GLOBAL orders.~n", [color:red("RECEIVE TIMEOUT:")])
          end;

        {orders, LocalOrderList} ->
          [LocalOrder|_D] = LocalOrderList,
          elevatorman ! {order, LocalOrder}
      after ?RECEIVE_BLOCK_TIME -> io:format("~s Order distributor waiting for LOCAL orders.~n", [color:red("RECEIVE TIMEOUT:")])
      end
  end,
  distributor().

get_best_elevator(Order) ->
  Elevators = get_all_elevators(Order),
  CostList = lists:map(fun(Elevator) -> {abs(element(3, Elevator) - Order#order.floor), Elevator} end, Elevators),
  case CostList of
    [] ->
      Executor = [];
    CostList ->
      [{_Cost, Executor} | _Disregard] = lists:keysort(1, CostList)
  end,

  {Executor, Elevators--[Executor]}.

get_all_elevators(Order) ->
  ListCreator = spawn(fun() -> elevator_list([]) end),
  lists:foreach(fun(Node) ->
    {stateman, Node} ! {get_state, self()},
    receive {elevator_state, Elevator} ->
      case element(2, Elevator) of
        idle ->
          ListCreator ! {add_elevator, Elevator};
        busy ->
          if
            Order#order.direction == element(4, Elevator) ->
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
                  ListCreator ! {add_elevator, Elevator}
              end;
            true -> ok
          end;

        _ ->
          io:format("ORDER DISTRIBUTOR: elevator not idle or driving: ~p~n", [Elevator])
      end
    after 500 ->
      erlang:disconnect_node(Node)
    end
  end, [node()|nodes()]),

    ListCreator ! return_list,
    Elevators = receive {elevator_list, ElevatorList} -> ElevatorList
    after ?RECEIVE_BLOCK_TIME ->
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
