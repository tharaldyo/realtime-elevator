-module(elevator).
-export([start/0, state_manager/4]).

start() ->
	order_manager:start(),

	register(driverman, spawn(fun driver_manager/0)),

	connection_manager:start(),

	register(elevatorman, spawn(fun elevator_manager/0)),

	register(fsm,spawn(fun state_machine:start/0)),

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

		{set_motor, Direction} ->
			elev_driver:set_motor_direction(Direction);

		{open_door} ->
			elev_driver:set_door_open_lamp(on);

		{close_door} ->
			elev_driver:set_door_open_lamp(off);

		_Message ->
			io:format("~p~n", [_Message])
		end,
	driver_manager_loop().

elevator_manager() ->
	io:format("elevatorman init ~n"), %debug

	% make sure fsm and state manager initialize correctly
	receive {fsm, intializing} -> ok end,
	io:format("Setting motor dir down~n"),
	driverman ! {set_motor, down},
	receive {floor_reached, NewFloor} ->
		driverman ! {set_motor, stop},
		stateman ! {update_state, floor, NewFloor},
		fsm ! {floor_reached}
	end,

	% TODO: remember to test this with the elevator

	receive {fsm, initialized} -> ok end,

	io:format("Elevator initialized, ready for action. ~n"), %debug

	elevator_manager_loop().

elevator_manager_loop() ->
	receive
		{floor_reached, NewFloor} ->
			% set floor indicator light here
			stateman ! {update_state, floor, NewFloor};
			%stateman ! {get_state, self()},

		idle ->
			stateman ! {update_state, state, idle},
			% delay here to prevent multiple elevators attempting to invoke order distribution simultaneously
			% possible problem: elevators calling distributor at the same time when multiple elevators are idle?

			% this stuff below belongs in order_distributor
			distributor ! get_floor,
			io:format("sent get_floor to distributor, awaiting response ~n"),

			receive
				[] ->
					io:format("received empty list, no orders available OR no order for me"); %debug

				OrderFloor ->
					io:format("received an order floor: ~p~n", [OrderFloor]),
					stateman ! {update_state, state, busy},
					stateman ! get_state,
					CurrentFloor = receive {_Name, _State, Floor, _Direction} -> Floor end,
					if
						CurrentFloor - OrderFloor == 0 ->
							driverman ! open_door;
						CurrentFloor - OrderFloor < 0 ->
							driverman ! {set_motor, up},
							stateman ! {update_state, direction, up};
						CurrentFloor - OrderFloor > 0 ->
							driverman ! {set_motor, down},
							stateman ! {update_state, direction, down}
					end
					%orderman ! {remove_order, MyOrder} % do this somewhere else
			end
		end,

	elevator_manager_loop().

state_manager(NodeName, State, Floor, Direction) ->
	io:format("statemanager has been called ~n"), %debug
	%nameman ! {get_name, self()},


		receive
			{node_name, NewName} ->
				state_manager(NewName, State, Floor, Direction);

			{update_state, state, NewState} ->
				state_manager(NodeName, NewState, Floor, Direction);

			{update_state, floor, NewFloor} ->
				state_manager(NodeName, State, NewFloor, Direction);

			{update_state, direction, NewDirection} ->
				state_manager(NodeName, State, Floor, NewDirection);

			{get_state, Receiver} ->
				Receiver ! {NodeName, State, Floor, Direction},
				state_manager(NodeName, State, Floor, Direction)
			end.
