-module(elevator).
-record(order, {floor, direction}).
-include("constants.hrl").

-export([ start/0,
					state_manager/5 ]).

start() ->
	order_manager:start(),
	register(driverman, spawn(fun driver_manager/0)),
	connection_manager:start(),
	register(elevatorman, spawn(fun elevator_manager/0)),
	register(fsm,spawn(fun state_machine:start/0)),
	register(stateman, spawn(?MODULE, state_manager, [placeholder, init, -1, down, {#order{floor = -1, direction = down}}])),
	nameman ! {get_name, stateman},
	register(distributor, spawn(fun order_distributor:start/0)),
	register(watchdog, spawn(fun watchdog/0)).

driver_manager() ->
	elev_driver:start(driverman, elevator),
	timer:sleep(1000),
	elevatorman ! {driverman, initialized},
	driver_manager_loop().

driver_manager_loop() ->
	receive
		{new_order, Floor, Direction} ->
		  order_manager:add_order(Floor, Direction);

		{floor_reached, 0} ->
			elev_driver:set_motor_direction(stop),
			elevatorman ! {floor_reached, 0};

		{floor_reached, 3} ->
			elev_driver:set_motor_direction(stop),
			elevatorman ! {floor_reached, 3};

		{floor_reached, Floor} ->
			elevatorman ! {floor_reached, Floor};

		{set_motor, Direction} ->
			elev_driver:set_motor_direction(Direction);

		open_door ->
			elev_driver:set_door_open_lamp(on);

		close_door ->
			elev_driver:set_door_open_lamp(off);

		{set_hall_lamp, Floor, Direction, State} ->
			elev_driver:set_button_lamp(Floor, Direction, State);

		{set_cab_lamp, Floor, State} ->
			elev_driver:set_button_lamp(Floor, command, State)

		end,
	driver_manager_loop().

% Initialization routine
elevator_manager() ->
	receive {driverman, initialized} -> ok end,
	receive {fsm, intializing} -> ok end,
	driverman ! {set_motor, down},
	receive {floor_reached, NewFloor} ->
		driverman ! {set_motor, stop},
		stateman ! {update_state, floor, NewFloor},
		fsm ! floor_reached
	end,

	receive {fsm, initialized} -> ok end,
	elevator_manager_loop().

elevator_manager_loop() ->
	receive
		{floor_reached, NewFloor} ->
			stateman ! {update_state, floor, NewFloor},

			stateman ! {get_current_order, self()},
			CurrentOrder = receive {current_order, O} -> O
			after ?RECEIVE_BLOCK_TIME -> io:format("~s: elevatorman was waiting for current order~n", [color:red("TIMEOUT")]) end,

			TargetFloor = CurrentOrder#order.floor,
			stateman ! {get_direction, self()},
			Direction = receive {direction, D} -> D
			after ?RECEIVE_BLOCK_TIME -> io:format("~s: elevatorman was waiting for direction~n", [color:red("TIMEOUT")]) end,

			% Construct a list over cab orders at the new floor
			localorderman ! {get_orders, self()},
			receive {orders, LocalOrders} ->
				LocalOrdersOnFloor = lists:filter(fun({_A,Floor,_D}) -> (Floor==NewFloor) end, LocalOrders)
			after ?RECEIVE_BLOCK_TIME ->
				io:format("~s: elevatorman was waiting for local orders~n", [color:red("TIMEOUT")]),
			 	LocalOrdersOnFloor = []
			end,

			% Construct a list of hall orders at the new floor
			orderman ! {get_orders, self()},
			receive {orders, GlobalOrders} ->
				GlobalOrdersOnFloor = lists:filter(fun({_A,Floor,_D}) -> (Floor==NewFloor) end, GlobalOrders)
				after ?RECEIVE_BLOCK_TIME ->
					io:format("~s: elevatorman was waiting for global orders~n", [color:red("TIMEOUT")]),
				 	GlobalOrdersOnFloor = []
				end,

			case NewFloor of
				TargetFloor ->
					fsm ! floor_reached,

					case CurrentOrder#order.direction of
						command -> ok;
						_D -> watchdog ! {elevator, remove_order, CurrentOrder}
					end,

					turn_all_lights_off(NewFloor),
					lists:foreach(fun(Order) -> order_manager:remove_order(localorderman, Order) end, LocalOrdersOnFloor),
					lists:foreach(fun(Order) -> order_manager:remove_order(orderman, Order) end, GlobalOrdersOnFloor);

				_OtherFloor ->
					fsm ! floor_passed,
					GlobalOrdersOnFloorInDirection = lists:filter(fun({_A,_F,Dir}) -> (Dir==Direction) end, GlobalOrdersOnFloor),

					case LocalOrdersOnFloor++GlobalOrdersOnFloorInDirection of
						[] ->
							io:format("ELEVATOR: No orders at this floor :-) ~n");
						_StuffAtThisFloor ->
							driverman ! {set_motor, stop},
							driverman ! open_door,
							lists:foreach(fun(Order) ->
								order_manager:remove_order(localorderman, Order),
								driverman ! {set_cab_lamp, Order#order.floor, off}
							end, LocalOrdersOnFloor),
							lists:foreach(fun(Order) ->
								lists:foreach(fun(Node) ->
									{driverman, Node} ! {set_hall_lamp, Order#order.floor, Direction, off}
								end, [node()|nodes()]),
								order_manager:remove_order(orderman, Order)
							end, GlobalOrdersOnFloorInDirection),

							timer:sleep(?DOOR_OPEN_TIME),
							driverman ! close_door,
							driverman ! {set_motor, Direction}
						end
			end;

		idle ->
			stateman ! {update_state, state, idle},
			distributor ! get_order,

			receive
				{order, []} ->
					flusher({order, []}),
					elevator_manager_loop();

				{order, Order} ->
					stateman ! {update_state, state, busy},
					stateman ! {update_state, order, Order},

					case Order#order.direction of
						command -> ok;
						_D -> watchdog ! {elevator, add_order, Order}
					end,

					OrderFloor = Order#order.floor,
					stateman ! {get_current_floor, self()},
					CurrentFloor = receive {current_floor, Floor} -> Floor
					after ?RECEIVE_BLOCK_TIME ->
						io:format("~s: elevatorman was waiting for current floor~n", [color:red("TIMEOUT")]) end,

					if
						CurrentFloor == OrderFloor ->
							elevatorman ! {floor_reached, CurrentFloor};
						CurrentFloor < OrderFloor ->
							stateman ! {update_state, direction, up},
							driverman ! {set_motor, up},
							fsm ! {drive, up};
						CurrentFloor > OrderFloor ->
							stateman ! {update_state, direction, down},
							driverman ! {set_motor, down},
							fsm ! {drive, down}
					end
			after ?RECEIVE_BLOCK_TIME -> io:format("~s: elevatorman was waiting for order from distributor~n", [color:red("TIMEOUT")]) end
		end,
	elevator_manager_loop().

flusher(ToFlush) ->
	receive
		ToFlush -> flusher(ToFlush)
	after 0 -> ok
	end.

turn_all_lights_off(Floor) ->
	case Floor of
		0 ->
			driverman ! {set_hall_lamp, Floor, command, off},
			lists:foreach(fun(Node) -> {driverman, Node} ! {set_hall_lamp, Floor, up, off} end, [node()|nodes()]);
		3 ->
			driverman ! {set_hall_lamp, Floor, command, off},
			lists:foreach(fun(Node) -> {driverman, Node} ! {set_hall_lamp, Floor, down, off} end, [node()|nodes()]);
		_else ->
			driverman ! {set_cab_lamp, Floor, off},
			lists:foreach(fun(Node) ->
				{driverman, Node} ! {set_hall_lamp, Floor, down, off},
				{driverman, Node} ! {set_hall_lamp, Floor, up, off}
			 end, [node()|nodes()])
	end.

watchdog() ->
	watchdog_loop([]).

 watchdog_loop(WatcherList) ->
	receive
		{elevator, Action, Order} ->
			lists:foreach(fun(Node) ->
				 {watchdog, Node} ! {network, Action, Order}
			 end, [node()|nodes()]),
			watchdog_loop(WatcherList);

		{network, add_order, Order} ->
			PID = spawn(fun() -> watcher_process(Order) end),
			watchdog_loop(WatcherList++[{PID, Order}]);

		{network, remove_order, Order} ->
			WatcherTuple = lists:keyfind(Order, 2, WatcherList),
			case WatcherTuple of
				false ->
					watchdog_loop(WatcherList);
				Watcher ->
					WatcherPID = element(1, Watcher),
					WatcherPID ! completed,
					watchdog_loop(WatcherList--[Watcher])
			end
	end.

watcher_process(Order) ->
	receive
		completed ->
			ok
	after
		20000 ->
			order_manager:add_order(Order#order.floor, Order#order.direction),
			watchdog ! {elevator, remove_order, Order}

	end.

state_manager(NodeName, State, Floor, Direction, Order) ->
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

			{update_state, order, NewOrder} ->
				state_manager(NodeName, State, Floor, Direction, NewOrder);

			{get_state, Receiver} ->
				Receiver ! {elevator_state, {NodeName, State, Floor, Direction, Order}},
				state_manager(NodeName, State, Floor, Direction, Order);

			{get_current_order, Receiver} ->
				Receiver ! {current_order, Order},
				state_manager(NodeName, State, Floor, Direction, Order);

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
