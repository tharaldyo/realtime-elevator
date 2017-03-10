-module(elevator).
-export([start/0, state_manager/5]).

start() ->
	order_manager:start(),
	register(driverman, spawn(fun driver_manager/0)),
	connection_manager:start(),
	register(elevatorman, spawn(fun elevator_manager/0)),
	register(fsm,spawn(fun state_machine:start/0)),
	register(stateman, spawn(?MODULE, state_manager, [placeholder, init, -1, down, -1])),
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
			io:format("new_order: ~p~n", [Floor]),
		  order_manager:add_order(Floor, Direction);

		{floor_reached, 0} -> % arbitrary?
			elev_driver:set_motor_direction(stop),
			elevatorman ! {floor_reached, 0};

		{floor_reached, Floor} ->
			elevatorman ! {floor_reached, Floor};

		{set_motor, Direction} ->
			elev_driver:set_motor_direction(Direction);

		{open_door} ->
			elev_driver:set_door_open_lamp(on);

		{close_door} ->
			elev_driver:set_door_open_lamp(off);

		_Message ->
			io:format("driverman received an abnormal message: ~p~n", [_Message]) %debug
		end,
	driver_manager_loop().

elevator_manager() ->
	% make sure fsm and state manager initialize correctly
	receive {driverman, initialized} -> ok end,
	receive {fsm, intializing} -> ok end,
	io:format("Setting motor dir down~n"), %debug

	driverman ! {set_motor, down},
	receive {floor_reached, NewFloor} ->
		driverman ! {set_motor, stop},
		stateman ! {update_state, floor, NewFloor},
		fsm ! {floor_reached}
	end,

	receive {fsm, initialized} -> ok end,
	io:format("Elevator initialized, ready for action. ~n"), %debug
	elevator_manager_loop().

elevator_manager_loop() ->
	receive
		{floor_reached, NewFloor} ->
			io:format("FLOOR ~p ---------------------- ~n", [NewFloor]),
			stateman ! {update_state, floor, NewFloor},
			stateman ! {get_floor, self()},
			TargetFloor = receive F -> F end,
			stateman ! {get_direction, self()},
			Direction = receive D -> D end,

			case NewFloor of
				TargetFloor ->
					fsm ! floor_reached,
					io:format("~s~n", [color:red("TARGET FLOOR REACHED.")]),
					driverman ! {set_motor, stop};
					%clear_all_floors_at(NewFloor)
				_ ->
					fsm ! floor_passed,
					% TODO: We also need to take care of "local" orders!
					% Are there orders here going in the same direction?
					% In this case we wish to stop, and take new orders.
					% We then need to open door, give them some seconds to enter new order,
					% and then close doors, and continue upwards.
					localorderman ! {get_orders, self()},

					receive
						{orders, LocalOrders} ->
							io:format("Orders: ~p~n", [LocalOrders]),
							LocalOrdersInSameDir = lists:filter(fun({_A,Floor,Dir}) -> (Dir==command) and (Floor==NewFloor) end, LocalOrders),
							io:format("Adding: ~p~n", [LocalOrdersInSameDir])
					end,

					orderman ! {get_orders, self()},

					receive
						{orders, GlobalOrders} ->
							io:format("Orders: ~p~n", [GlobalOrders]),
							GlobalOrdersInSameDir = lists:filter(fun({_A,Floor,Dir}) -> (Dir==Direction) and (Floor==NewFloor) end, GlobalOrders),
							io:format("Adding: ~p~n", [GlobalOrdersInSameDir])
					end,

					% Now have a list of orders in same dir. If there are any orders in this list
					% we want to stop, open door, continue on! Consider putting code below
					% into a "open_for_new_passengers"-function
					io:format("~s, ~p~n", [color:red("Orders I remove at this floor:"), LocalOrdersInSameDir++GlobalOrdersInSameDir]),
					case LocalOrdersInSameDir++GlobalOrdersInSameDir of
						[] ->
							io:format("No orders at this floor :-) ~n");
						_StuffAtThisFloor ->
							driverman ! {set_motor, stop},
							driverman ! {open_door},
							timer:sleep(3000),
							driverman ! {close_door},
							lists:foreach(fun(Order) -> order_manager:remove_order(localorderman, Order) end, LocalOrdersInSameDir),
							lists:foreach(fun(Order) -> order_manager:remove_order(orderman, Order) end, GlobalOrdersInSameDir),
							driverman ! {set_motor, Direction},
							io:format("~s, ~p~n", [color:red("Finished: "), LocalOrdersInSameDir++GlobalOrdersInSameDir])
						end
			end;

		idle ->
			stateman ! {update_state, state, idle},
			stateman ! {update_state, target_floor, -1},
			% delay here to prevent multiple elevators attempting to invoke order distribution simultaneously
			% possible problem: elevators calling distributor at the same time when multiple elevators are idle?

			% this stuff below belongs in order_distributor
			distributor ! get_floor,
			%io:format("sent get_floor to distributor, awaiting response ~n"), %debug

			receive
				{order, []} ->
					io:format("~n");
					%io:format("received empty list, no orders available OR no order for me ~n"); %debug

				{order, OrderFloor} ->
					%io:format("received an order floor: ~p~n", [OrderFloor]),
					stateman ! {update_state, state, busy},
					stateman ! {update_state, target_floor, OrderFloor},
					% write OrderFloor to disk?
					% then delete it from the orderlist in order_manager
					stateman ! {get_state, self()},
					CurrentFloor = receive {_Name, _State, Floor, _Direction, _Target} -> Floor end,
					if
						CurrentFloor - OrderFloor == 0 ->
							fsm ! floor_reached;
						CurrentFloor - OrderFloor < 0 ->
							fsm ! {drive, up},
							stateman ! {update_state, direction, up};
						CurrentFloor - OrderFloor > 0 ->
							fsm ! {drive, down},
							stateman ! {update_state, direction, down}
					end
					%orderman ! {remove_order, MyOrder} % do this somewhere else
			end
		end,
	elevator_manager_loop().

state_manager(NodeName, State, Floor, Direction, TargetFloor) ->
	io:format("statemanager has been called ~n"), %debug
	%nameman ! {get_name, self()},

		receive
			{node_name, NewName} ->
				state_manager(NewName, State, Floor, Direction, TargetFloor);

			{update_state, state, NewState} ->
				state_manager(NodeName, NewState, Floor, Direction, TargetFloor);

			{update_state, floor, NewFloor} ->
				elev_driver:set_floor_indicator(NewFloor),
				state_manager(NodeName, State, NewFloor, Direction, TargetFloor);

			{update_state, direction, NewDirection} ->
				state_manager(NodeName, State, Floor, NewDirection, TargetFloor);

			{update_state, target_floor, NewTargetFloor} ->
				state_manager(NodeName, State, Floor, Direction, NewTargetFloor);

			{get_state, Receiver} ->
				Receiver ! {NodeName, State, Floor, Direction, TargetFloor},
				state_manager(NodeName, State, Floor, Direction, TargetFloor);

			{get_target_floor, Receiver} ->
				Receiver ! TargetFloor;

			{get_direction, Receiver} ->
				Receiver ! Direction;

			{get_floor, Receiver} ->
				Receiver ! Floor
			end.
