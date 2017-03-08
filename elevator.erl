-module(elevator).
-export([start/0, state_manager/4]).

start() ->
	order_manager:start(),

	register(driverman, spawn(fun driver_manager/0)),

	connection_manager:start(),

	register(elevatorman, spawn(fun elevator_manager/0)),

	register(fsm, state_machine:start()),

	%nameman ! {get_name, self()},
	%NodeName = receive _ -> NodeName end,

	register(stateman, spawn(?MODULE, state_manager, [placeholder, init, -1, down])),
	nameman ! {get_name, stateman},

	register(distributor, spawn(fun order_distributor:start/0)),

	io:format("Elevator pid: ~p~n", [self()]).

driver_manager() ->
	elev_driver:start(driverman, elevator),
	io:format("driverman initialized ~n"), %debug
	timer:sleep(1000), %debug: try to wait for elevatorman
	elevatorman ! {driverman, initialized},
	driver_manager_loop().

driver_manager_loop() ->
	receive
		{new_order, Floor, Direction} -> % TODO: do pattern matching for "command" somewhere
			io:format("new_order received in driver manager ~n"),
		  order_manager:add_order(Floor, Direction);

		{floor_reached, 0} -> % arbitrary?
			elev_driver:set_motor_direction(stop),
			elevatorman ! {floor_reached, 0};

		{floor_reached, Floor} ->
			elevatorman ! {floor_reached, Floor};

		_Message ->
			io:format("~p~n", [_Message])
		end,
	driver_manager_loop().

elevator_manager() ->
	io:format("elevatorman init ~n"), %debug

	% make sure fsm and state manager initialize correctly
	receive {fsm, intializing} -> ok end,
	receive {stateman, initialized} -> ok end,

	driverman ! {set_motor, down},
	receive {floor_reached, NewFloor} ->
		elevatorman ! {set_motor, stop},
		stateman ! {update_state, floor, NewFloor},
		fsm ! {floor_reached}
	end,

	timer:sleep(500), %debug
	io:format("Elevator initialized, ready for action. ~n"), %debug

	elevator_manager_loop().

elevator_manager_loop() ->
	receive
		{floor_reached, NewFloor} ->
			% set floor indicator light here
			stateman ! {update_state, floor, NewFloor};
			%stateman ! {get_state, self()},

		{idle} -> %logic for distributing orders?
			orderman ! {get_orders, self()},
			receive
				[] ->
					io:format("received empty list, no orders available"); %debug
				OrderList ->
					io:format("Orders received: ~p~n", [OrderList]), %debug
					[MyOrder|_Disregard] = OrderList,
					io:format("my order: ~p~n", [MyOrder]), %debug
					stateman ! get_state,
					CurrentFloor = receive {_Name, _State, Floor, _Direction} -> Floor end,
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

state_manager(NodeName, State, Floor, Direction) ->
	io:format("hello from state manager ~n"), %debug
	%nameman ! {get_name, self()},


		receive
			{node_name, NewName} ->
				state_manager(NewName, State, Floor, Direction);

			{update_state, floor, NewFloor} ->
				state_manager(NodeName, State, NewFloor, Direction);

			{update_state, direction, NewDirection} ->
				state_manager(NodeName, State, Floor, NewDirection);

			{get_state, Receiver} ->
				Receiver ! {NodeName, State, Floor, Direction},
				state_manager(NodeName, State, Floor, Direction)
			end.
