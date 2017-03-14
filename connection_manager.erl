-module(connection_manager).
-define(RECEIVE_PORT, 20066).
-define(SEND_PORT, 20067).

-export([ start/0,
					name_manager/1 ]).

start() ->
	node_init(),
	spawn(fun listen/0),
	spawn(fun broadcast/0).

node_init() ->
	os:cmd("epmd -daemon"),
	timer:sleep(100),
	{_ok, [{IPtuple, _Broadcast, _Self} | _Disregard]} = inet:getif(),
	NodeName = "elevator@"++integer_to_list(element(1,IPtuple))++"."++integer_to_list(element(2,IPtuple))++"."++integer_to_list(element(3,IPtuple))++"."++integer_to_list(element(4,IPtuple)),
	register(nameman, spawn(fun() -> name_manager(NodeName) end)),
	net_kernel:start([list_to_atom(NodeName), longnames]),
	erlang:set_cookie(node(), 'what-is-network-security?').

listen() ->
	{ok, ReceiveSocket} = gen_udp:open(?RECEIVE_PORT, [list, {active, false}]),
	listen(ReceiveSocket).

listen(ReceiveSocket) ->
	{ok, {_Address, _Port, NodeName}} = gen_udp:recv(ReceiveSocket, 0),
	Node = list_to_atom(NodeName),
	case lists:member(Node, [node()|nodes()]) of
		true ->
			listen(ReceiveSocket);
		false ->
			net_adm:ping(Node),
			listen(ReceiveSocket)
		end.

broadcast() ->
	{ok, SendSocket} = gen_udp:open(?SEND_PORT, [list, {active, true}, {broadcast, true}]),
	broadcast(SendSocket).

broadcast(SendSocket) ->
	ok = gen_udp:send(SendSocket, {255,255,255,255}, ?RECEIVE_PORT, atom_to_list(node())),
	timer:sleep(7000),
	broadcast(SendSocket).

name_manager(NodeName) ->
	receive {get_name, PID} ->
		PID ! {node_name, NodeName}
	end,
	name_manager(NodeName).
