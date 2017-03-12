-module(elevator).
-export([start/0, state_manager/5]).
-record(order, {floor, direction}).
-include("constants.hrl").

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
	io:format("ELEVATOR: Elevator pid: ~p~n", [self()]).

driver_manager() ->
	elev_driver:start(driverman, elevator), % 'elevator' or 'simulator'
	timer:sleep(1000), %debug: try to wait for elevatorman
	elevatorman ! {driverman, initialized},
	driver_manager_loop().

driver_manager_loop() ->
	receive
		{new_order, Floor, Direction} -> % TODO: do pattern matching for "command" somewhere
			io:format("DRIVER MANAGER: new_order: ~p~n", [Floor]),
		  order_manager:add_order(Floor, Direction);

		{floor_reached, 0} -> % arbitrary?
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

		%{set_hall_lamp, Floor, Direction, State} when State == off ->
		%	% need to distribute this command over network
		%	elev_driver:set_hall_lamp(Floor, Direction, State),
		%	lists:foreach(fun(Node) ->  {Node, driverman} ! {set_hall_lamp, Floor, Direction, State}    end, [Nodes()])

		{set_hall_lamp, Floor, Direction, State} ->
			%io:format("Setting light at ~p, ~p, ~p~n", [Floor, Direction, State]),
			elev_driver:set_button_lamp(Floor, Direction, State);

		{set_cab_lamp, Floor, State} ->
			%io:format("I received the order to turn off the cab light.~n"),
			elev_driver:set_button_lamp(Floor, command, State);

		_Message ->
			io:format("DRIVER MANAGER: received an abnormal message: ~p~n", [_Message]) %debug
		end,
	driver_manager_loop().

elevator_manager() ->
	% make sure fsm and state manager initialize correctly
	receive {driverman, initialized} -> ok end,
	receive {fsm, intializing} -> ok end,
	io:format("~s~n", [color:green("ELEVATOR: Driverman and FSM initialized.")]), %debug

	driverman ! {set_motor, down},
	receive {floor_reached, NewFloor} ->
		driverman ! {set_motor, stop},
		stateman ! {update_state, floor, NewFloor},
		fsm ! floor_reached
	end,

	receive {fsm, initialized} -> ok end,
	io:format("ELEVATOR: Elevator initialized, ready for action. ~n"), %debug
	elevator_manager_loop().

elevator_manager_loop() ->
	receive
		{floor_reached, NewFloor} ->
			io:format("FLOOR ~p ---------------------- ~n", [NewFloor]),
			stateman ! {update_state, floor, NewFloor},

			stateman ! {get_current_order, self()},
			CurrentOrder = receive {current_order, O} -> O
			after ?RECEIVE_BLOCK_TIME -> io:format("~s: elevatorman was waiting for current order~n", [color:red("TIMEOUT")]) end,

			io:format("ELEVATOR MANAGER: Current order: ~p~n", [CurrentOrder]),
			TargetFloor = CurrentOrder#order.floor,
			io:format("ELEVATOR MANAGER: Target floor: ~p~n", [TargetFloor]),
			stateman ! {get_direction, self()},
			Direction = receive {direction, D} -> D
			after ?RECEIVE_BLOCK_TIME -> io:format("~s: elevatorman was waiting for direction~n", [color:red("TIMEOUT")]) end,
			%io:format("ready to match case ~n"),

			% Construct a list over cab orders at the new floor
			localorderman ! {get_orders, self()},
			receive {orders, LocalOrders} ->
				%io:format("ELEVATOR: Orders: ~p~n", [LocalOrders]),
				LocalOrdersOnFloor = lists:filter(fun({_A,Floor,_D}) -> (Floor==NewFloor) end, LocalOrders)
			after ?RECEIVE_BLOCK_TIME ->
				io:format("~s: elevatorman was waiting for local orders~n", [color:red("TIMEOUT")]),
			 	LocalOrdersOnFloor = []
			end,

			% Construct a list over hall orders at the new floor
			orderman ! {get_orders, self()},
			receive {orders, GlobalOrders} ->
				%io:format("ELEVATOR: Orders: ~p~n", [GlobalOrders]),
				GlobalOrdersOnFloor = lists:filter(fun({_A,Floor,_D}) -> (Floor==NewFloor) end, GlobalOrders)
				after ?RECEIVE_BLOCK_TIME ->
					io:format("~s: elevatorman was waiting for global orders~n", [color:red("TIMEOUT")]),
				 	GlobalOrdersOnFloor = []
				end,

			case NewFloor of
				TargetFloor ->
					fsm ! floor_reached,
					%driverman ! {set_motor, stop}, %redundant?
					io:format("~s~n", [color:red("TARGET FLOOR REACHED!")]),

					case CurrentOrder#order.direction of
						command -> ok;
						_D -> watchdog ! {elevator, remove_order, CurrentOrder}
					end,
					%	watchdog ! {elevator, remove_order, CurrentOrder},
					% what about orders it clears during travel? ref. below
					% ^ these orders should not be in the watchlist in the first place

					%driverman ! {set_cab_lamp, NewFloor, off},
					turn_all_lights_off(NewFloor),

					lists:foreach(fun(Order) -> order_manager:remove_order(localorderman, Order) end, LocalOrdersOnFloor),
					lists:foreach(fun(Order) -> order_manager:remove_order(orderman, Order) end, GlobalOrdersOnFloor),
					io:format("ELEVATOR: Additionally, I remove these orders from target floor: ~p~n", [LocalOrdersOnFloor++GlobalOrdersOnFloor]);

				_OtherFloor ->
					fsm ! floor_passed,

					GlobalOrdersOnFloorInDirection = lists:filter(fun({_A,_F,Dir}) -> (Dir==Direction) end, GlobalOrdersOnFloor),

					io:format("~s, ~p~n", [color:red("Orders I remove at this floor:"), LocalOrdersOnFloor++GlobalOrdersOnFloorInDirection]),
					case LocalOrdersOnFloor++GlobalOrdersOnFloorInDirection of
						[] ->
							io:format("ELEVATOR: No orders at this floor :-) ~n");
						_StuffAtThisFloor ->
							driverman ! {set_motor, stop},
							%driverman ! {set_hall_lamp, NewFloor, Direction, off},
							driverman ! open_door,
							lists:foreach(fun(Order) ->
								order_manager:remove_order(localorderman, Order),
								driverman ! {set_cab_lamp, Order#order.floor, off}
							end, LocalOrdersOnFloor),
							%lists:foreach(fun(Order) -> io:format("driverman instructions ~p,~p~n", [Order#order.floor, Direction]) end, GlobalOrdersOnFloorInDirection), %debug
							lists:foreach(fun(Order) ->
								lists:foreach(fun(Node) ->
									{driverman, Node} ! {set_hall_lamp, Order#order.floor, Direction, off}
								end, [node()|nodes()]),
								order_manager:remove_order(orderman, Order)
							end, GlobalOrdersOnFloorInDirection),

							timer:sleep(?DOOR_OPEN_TIME),
							driverman ! close_door,
							driverman ! {set_motor, Direction},
							io:format("~s, ~p~n", [color:red("Finished: "), LocalOrdersOnFloor++GlobalOrdersOnFloorInDirection])
						end
			end;

		idle ->
			io:format("ELEVATOR: elevatorman received idle message, updating state and asking for order ~n"),
			stateman ! {update_state, state, idle},
			% delay here to prevent multiple elevators attempting to invoke order distribution simultaneously
			% possible problem: elevators calling distributor at the same time when multiple elevators are idle?
			distributor ! get_order,
			%io:format("sent get_order to distributor, awaiting response ~n"), %debug

			receive
				{order, []} ->
					flusher({order, []}),
					%io:format("~n"),
					io:format("ELEVATOR: received empty list, no orders available OR no order for me ~n"), %debug
					elevator_manager_loop();

				{order, Order} ->
					stateman ! {update_state, state, busy},
					stateman ! {update_state, order, Order},
					io:format("ELEVATOR: received an order: ~p~n", [Order]),

					case Order#order.direction of
						command -> ok;
						_D -> watchdog ! {elevator, add_order, Order}
					end,

					OrderFloor = Order#order.floor,

					% write OrderFloor to disk?
					% then delete it from the orderlist in order_manager
					stateman ! {get_current_floor, self()},
					CurrentFloor = receive {current_floor, Floor} -> Floor
					after ?RECEIVE_BLOCK_TIME ->
						io:format("~s: elevatorman was waiting for current floor~n", [color:red("TIMEOUT")]) end,

					if % TODO: check if all states are updated correctly
						CurrentFloor == OrderFloor ->
							io:format("ELEVATOR: order is on my current floor, sending floor_reached right away ~n"),
							elevatorman ! {floor_reached, CurrentFloor};
						CurrentFloor < OrderFloor ->
							io:format("ELEVATOR: order received, telling FSM to start driving ASAP ~n"),
							stateman ! {update_state, direction, up},
							driverman ! {set_motor, up},
							fsm ! {drive, up};
						CurrentFloor > OrderFloor ->
							io:format("ELEVATOR: order received, telling FSM to start driving ASAP ~n"),
							stateman ! {update_state, direction, down},
							driverman ! {set_motor, down},
							fsm ! {drive, down}
					end
					%orderman ! {remove_order, MyOrder} % do this somewhere else
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
		_else -> % 1 or 2
			driverman ! {set_cab_lamp, Floor, off},
			lists:foreach(fun(Node) ->
				{driverman, Node} ! {set_hall_lamp, Floor, down, off},
				{driverman, Node} ! {set_hall_lamp, Floor, up, off}
			 end, [node()|nodes()])
	end.

watchdog() ->
	io:format("WATCHDOG: watchdog initialized ~n"),
	watchdog_loop([]).

watchdog_loop(WatcherList) ->
	io:format("WATCHDOG: this is the current WatcherList: ~p~n", [WatcherList]),

	receive
		{elevator, Action, Order} -> % Action is either add_order or remove_order
			lists:foreach(fun(Node) ->
				 {watchdog, Node} ! {network, Action, Order},
				 io:format("WATCHDOG: Broadcasting action '~p' of order '~p' to node '~p'~n", [Action, Order, Node])
			 end, [node()|nodes()]),
			watchdog_loop(WatcherList);

		{network, add_order, Order} ->
			PID = spawn(fun() -> watcher_process(Order) end),
			io:format("WATCHDOG: Adding this to the WatcherList: ~p~n", [{PID, Order}]),
			watchdog_loop(WatcherList++[{PID, Order}]);

		{network, remove_order, Order} ->
			WatcherTuple = lists:keyfind(Order, 2, WatcherList),
			case WatcherTuple of
				false ->
					io:format("WATCHDOG: Error: this order is not being watched: ~p~n", [Order]),
					watchdog_loop(WatcherList);
				Watcher ->
					io:format("WATCHDOG: this watcher is ending: ~p~n", [Watcher]),
					WatcherPID = element(1, Watcher), % can do this and send message in one line instead
					WatcherPID ! completed,
					watchdog_loop(WatcherList--[Watcher])
			end
	end.

watcher_process(Order) ->
	receive
		completed ->
			io:format("WATCHER: order completed normally, exiting ~n"),
			ok
	after
		20000 -> % TODO: tweak this?
			io:format("WATCHER: order took too long, adding it back to the queue! ~n"),
			order_manager:add_order(Order#order.floor, Order#order.direction),
			watchdog ! {elevator, remove_order, Order}

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
