-module(elevator).
-export([start/0, state_manager/3]).

start() ->
	order_manager:start(),

	register(driverman, spawn(fun driver_manager/0)),

	connection_manager:start(),

	register(elevatorman, spawn(fun elevator_manager/0)),

	register(fsm, state_machine:start()),

	register(stateman, spawn(?MODULE, state_manager, [init, -1, down])),

	io:format("Elevator pid: ~p~n", [self()]).

driver_manager() ->
	elev_driver:start(driverman, elevator),
	io:format("driverman initialized ~n"), %debug
	driver_manager_loop().

driver_manager_loop() ->
	receive
		{new_order, Floor, Direction} -> % TODO: do pattern matching for "command" somewhere
			io:format("new_order received in driver manager ~n"),
		  order_manager:add_order(Floor, Direction);
		{floor_reached, 0} ->
			elev_driver:set_motor_direction(stop);
		{floor_reached, Floor} ->
			stateman ! {update_state, floor, Floor};
		_Message ->
			io:format("~p~n", [_Message])
		end,
	driver_manager_loop().

elevator_manager() ->
	io:format("elevatorman init ~n"), %debug

	receive
		init_started -> ok
	end,



	timer:sleep(5000), %debug
%	receive
%		{init_started} ->
	elevator_manager_loop().

elevator_manager_loop() ->
	receive
		{floor_reached, NewFloor} ->
			% set floor indicator light here
			stateman ! {update_state, floor, NewFloor};
			%stateman ! {get_state, self()},

		{idle} ->
			orderman ! {get_orders, self()},
			receive
				[] ->
					io:format("received empty list, no orders available"); %debug
				OrderList ->
					io:format("Orders received: ~p~n", [OrderList]), %debug
					[MyOrder|_Disregard] = OrderList,
					io:format("my order: ~p~n", [MyOrder]), %debug
					stateman ! get_state,
					CurrentFloor = receive % TODO: clean up the names around here
						{_State, Floor, _Direction} ->
							Floor
						end,
					Difference = CurrentFloor - element(2, MyOrder),
					if
						Difference < 0 ->
							io:format("something ~n");
						Difference > 0 ->
							io:format("something ~n")
					end

					%orderman ! {remove_order, MyOrder} % do this somewhere else
			end
		end,

	timer:sleep(5000),
	elevator_manager_loop().

state_manager(State, Floor, Direction) ->
	io:format("hello from state manager ~n"), %debug
		receive
			{update_state, floor, NewFloor} ->
				state_manager(State, NewFloor, Direction);

			{update_state, direction, NewDirection} ->
				state_manager(State, Floor, NewDirection);

			{get_state, Receiver} ->
				Receiver ! {State, Floor, Direction}
			end.
