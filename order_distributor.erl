-module(order_distributor).
-export([start/0]).

start() ->
  io:format("order_distributor started ~n"),
  distributor().

distributor() ->
  io:format("hehe ~n"),

  receive
    get_floor -> % main "gimme new order" call
      Executor = find_best_elevator(),
      io:format("The executor: ~p~n", [Executor]) %debug
      %send the order to Executor
      %send "no order for you" to Elevators--Executor
  end.

find_best_elevator() ->
  Elevators = get_all_elevators(),
  io:format("Elevators: ~p~n", [Elevators]), %debug
  % check if any orders available, else send [] to the elevator making the request
  % get first order in queue (FIFO)
  % check if the order has direction "command" (comes from inside the elevator)
  % if yes, send order to "local" elevator
  % else, check states of elevators in Elevators
  % compare the floors of the idle elevators to the one in the order
  % assign the order by sending the floor number to the elevator who "wins"
  % send something to the other elevators to let them know they "lost" (i.e. [])

  some_elevator. %debug: should return the winning elevator

get_all_elevators() ->
  % returns a list of all the elevators and their state: [elevator1, elevator2, ..]
  ListCreator = spawn(fun() -> elevator_list([], self()) end),
  lists:foreach(fun(Node) ->
    {stateman, Node} ! {get_state, self()},
    receive Elevator ->
      ListCreator ! {add_elevator, Elevator} end
    end, [node()|nodes()]), %debug use [node()|nodes()]

    ListCreator ! return_list,
    Elevators = receive ElevatorList -> ElevatorList end,
    Elevators.

elevator_list(Elevators, PID) ->
  receive
    {add_elevator, Elevator} ->
      elevator_list(Elevators++[Elevator], PID);
    return_list ->
      PID ! Elevators
  end.




%get_states() ->
  %foreach(get_state, [node|nodes()]
