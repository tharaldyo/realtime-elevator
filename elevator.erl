-module(elevator).
-export([start/0, state_manager/5]).
-record(order, {floor, direction}).

start() ->
	order_manager:start(),
	register(driverman, spawn(fun driver_manager/0)),
	connection_manager:start(),
	register(elevatorman, spawn(fun elevator_manager/0)),
	register(fsm,spawn(fun state_machine:start/0)),
	register(stateman, spawn(?MODULE, state_manager, [placeholder, init, -1, down, {#order{floor = -1, direction = down}}])),
	nameman ! {get_name, stateman},
	register(distributor, spawn(fun order_distributor:start/0)),
	register(watchdog, spawn(fun watchdog/0)),
	io:format("Elevator pid: ~p~n", [self()]).

driver_manager() ->
	elev_driver:start(driverman, elevator),
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

		open_door ->
			elev_driver:set_door_open_lamp(on);

		close_door ->
			elev_driver:set_door_open_lamp(off);

		{set_button_lamp, Floor, Direction, State} ->
			io:format("Setting light at ~p, ~p, ~p~n", [Floor, Direction, State]),
			elev_driver:set_button_lamp(Floor, Direction, State);

		_Message ->
			io:format("Driverman received an abnormal message: ~p~n", [_Message]) %debug
		end,
	driver_manager_loop().

elevator_manager() ->
	% make sure fsm and state manager initialize correctly
	receive {driverman, initialized} -> ok end,
	receive {fsm, intializing} -> ok end,
	io:format("~s~n", [color:green("Driverman and FSM initialized.")]), %debug

	driverman ! {set_motor, down},
	receive {floor_reached, NewFloor} ->
		driverman ! {set_motor, stop},
		stateman ! {update_state, floor, NewFloor},
		fsm ! floor_reached
	end,

	receive {fsm, initialized} -> ok end,
	io:format("Elevator initialized, ready for action. ~n"), %debug
	elevator_manager_loop().

elevator_manager_loop() ->
	receive
		{floor_reached, NewFloor} ->
			io:format("FLOOR ~p ---------------------- ~n", [NewFloor]),
			stateman ! {update_state, floor, NewFloor},
			stateman ! {get_current_order, self()},
			CurrentOrder = receive {target_floor, O} -> O end,
			TargetFloor = CurrentOrder#order.floor,
			io:format("Current target floor: ~p~n", [TargetFloor]),
			stateman ! {get_direction, self()},
			Direction = receive {direction, D} -> D end,
			%io:format("ready to match case ~n"),

			localorderman ! {get_orders, self()},
			receive {orders, LocalOrders} ->
				io:format("Orders: ~p~n", [LocalOrders]),
				LocalOrdersOnFloor = lists:filter(fun({_A,Floor,_D}) -> (Floor==NewFloor) end, LocalOrders)
			end,

			orderman ! {get_orders, self()},
			receive {orders, GlobalOrders} ->
				io:format("Orders: ~p~n", [GlobalOrders]),
				GlobalOrdersOnFloor = lists:filter(fun({_A,Floor,_D}) -> (Floor==NewFloor) end, GlobalOrders)
			end,

			case NewFloor of
				TargetFloor ->
					io:format("~s~n", [color:red("TARGET FLOOR REACHED!")]),


					%driverman ! {set_button_lamp, TargetFloor, Direction, off},
					driverman ! {set_motor, stop}, %redundant?
					%clear_all_floors_at(NewFloor)
					watchdog ! {elevator, remove_order, CurrentOrder},
					lists:foreach(fun(Order) -> order_manager:remove_order(localorderman, Order) end, LocalOrdersOnFloor),
					lists:foreach(fun(Order) -> order_manager:remove_order(orderman, Order) end, GlobalOrdersOnFloor),
					io:format("Additionally, I remove these orders from target floor: ~p~n", [LocalOrdersOnFloor++GlobalOrdersOnFloor]),
					fsm ! floor_reached;

				_ ->
					fsm ! floor_passed,

					GlobalOrdersOnFloorInDirection = lists:filter(fun({_A,_F,Dir}) -> (Dir==Direction) end, GlobalOrdersOnFloor),

					io:format("~s, ~p~n", [color:red("Orders I remove at this floor:"), LocalOrdersOnFloor++GlobalOrdersOnFloorInDirection]),
					case LocalOrdersOnFloor++GlobalOrdersOnFloorInDirection of
						[] ->
							io:format("No orders at this floor :-) ~n");
						_StuffAtThisFloor ->
							driverman ! {set_motor, stop},
							%driverman ! {set_button_lamp, NewFloor, Direction, off},
							driverman ! open_door,
							timer:sleep(2000),
							driverman ! close_door,
							driverman ! {set_motor, Direction},
							io:format("elevman removing~n"),
							lists:foreach(fun(Order) -> order_manager:remove_order(localorderman, Order) end, LocalOrdersOnFloor),
							lists:foreach(fun(Order) -> order_manager:remove_order(orderman, Order) end, GlobalOrdersOnFloorInDirection),
							%io:format("DRIVING ON! <------ ~n"),
							io:format("~s, ~p~n", [color:red("Finished: "), LocalOrdersOnFloor++GlobalOrdersOnFloorInDirection])
						end
			end;

		idle ->
			io:format("elevatorman received idle message, updating state and asking for order ~n"),
			stateman ! {update_state, state, idle},
			%stateman ! {update_state, new_order, -1},
			% delay here to prevent multiple elevators attempting to invoke order distribution simultaneously
			% possible problem: elevators calling distributor at the same time when multiple elevators are idle?
			% this stuff below belongs in order_distributor
			distributor ! get_order,
			%io:format("sent get_order to distributor, awaiting response ~n"), %debug

			receive
				{order, []} ->
					%io:format("~n"),
					io:format("received empty list, no orders available OR no order for me ~n"); %debug

				{order, Order} ->
					io:format("received an order floor: ~p~n", [Order]),
					OrderFloor = Order#order.floor,
					stateman ! {update_state, state, busy},
					stateman ! {update_state, new_order, Order},
					% write OrderFloor to disk?
					% then delete it from the orderlist in order_manager
					stateman ! {get_current_floor, self()},
					CurrentFloor = receive {current_floor, Floor} -> Floor end,
					if
						CurrentFloor - OrderFloor == 0 ->
							io:format("order is on my floor WHEN RECEIVED, deleting ~n"),
							% TODO: also remove all command orders from this floor
							case Order#order.direction of
								command ->
									order_manager:remove_order(localorderman, Order);
								% TODO: write the local order to disk?
								Direction ->
									watchdog ! {elevator, add_order, Order},
									order_manager:remove_order(orderman, Order)
							end,
							fsm ! floor_reached;
						CurrentFloor - OrderFloor < 0 ->
							io:format("order received, telling FSM to start driving ASAP ~n"),
							fsm ! {drive, up};
							%stateman ! {update_state, direction, up};
						CurrentFloor - OrderFloor > 0 ->
							io:format("order received, telling FSM to start driving ASAP ~n"),
							fsm ! {drive, down}
							%stateman ! {update_state, direction, down}
					end
					%orderman ! {remove_order, MyOrder} % do this somewhere else
			end
		end,
	elevator_manager_loop().

watchdog() ->
	io:format("watchdog initialized ~n"),
	watchdog_loop([]).

watchdog_loop(WatcherList) ->
	io:format("WATCHDOG: this is the current WatcherList: ~p~n", [WatcherList]),

	receive
		{elevator, Action, Order} -> % Action is either add_order or remove_order
			lists:foreach(fun(Node) -> {watchdog, Node} ! {network, Action, Order} end, nodes()),
			watchdog_loop(WatcherList);

		{network, add_order, Order} ->
			PID = spawn(fun() -> watcher_process(Order) end),
			watchdog_loop(WatcherList++{PID, Order});

		{network, remove_order, Order} ->
			Watcher = lists:keyfind(Order, 2, WatcherList),
			WatcherPID = element(1, Watcher), % sort of redundant, can do this and send message in one line instead
			WatcherPID ! completed,
			watchdog_loop(WatcherList--[Watcher])

	end.

watcher_process(Order) ->
	receive
		completed ->
			io:format("WATCHER: order completed normally, exiting ~n"),
			ok
	after
		20000 -> % TODO: tweak this?
			io:format("WATCHER: order took too long, adding it back to the queue! ~n"),
			order_manager:add_order(Order#order.floor, Order#order.direction)
	end. % TODO: should die here automatically, maybe check if it does?

state_manager(NodeName, State, Floor, Direction, Order) ->
	%io:format("statemanager has been called ~n"), %debug
	%nameman ! {get_name, self()},
		receive
			{node_name, NewName} ->
				state_manager(NewName, State, Floor, Direction, Order);

			{update_state, state, NewState} ->
				state_manager(NodeName, NewState, Floor, Direction, Order);

			{update_state, floor, NewFloor} ->
				elev_driver:set_floor_indicator(NewFloor),
				state_manager(NodeName, State, NewFloor, Direction, Order);

			{update_state, direction, NewDirection} ->
				state_manager(NodeName, State, Floor, NewDirection, Order);

			{update_state, new_order, NewOrder} ->
				state_manager(NodeName, State, Floor, Direction, NewOrder);

			{get_state, Receiver} ->
				Receiver ! {elevator_state, {NodeName, State, Floor, Direction, Order}},
				state_manager(NodeName, State, Floor, Direction, Order);

			{get_current_order, Receiver} ->
				Receiver ! {current_order, Order};

			{get_target_floor, Receiver} ->
				Receiver ! {target_floor, Order#order.floor},
			state_manager(NodeName, State, Floor, Direction, Order);

			{get_direction, Receiver} ->
				Receiver ! {direction, Direction},
				state_manager(NodeName, State, Floor, Direction, Order);

			{get_current_floor, Receiver} ->
				Receiver ! {current_floor, Floor},
				state_manager(NodeName, State, Floor, Direction, Order)
			end.
