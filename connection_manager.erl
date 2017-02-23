-module(connection_manager).
-export([start/0]).

-define(RECEIVE_PORT, 20066).
-define(SEND_PORT, 20067).

start() ->
	spawn(fun listen/0),
	spawn(fun broadcast/0).

listen() ->
	{ok, ReceiveSocket} = gen_udp:open(?RECEIVE_PORT, [list, {active, false}]),
	listen(ReceiveSocket).

listen(ReceiveSocket) ->
	{ok, {_Address, _Port, NodeName}} = gen_udp:recv(ReceiveSocket, 0),
	io:format("NodeName: ~p~n", [NodeName]),
	case lists:member(list_to_atom(NodeName), [node()|nodes()]) of
		true ->
			listen(ReceiveSocket);
		false ->
			net_adm:ping(list_to_atom(NodeName)), % ping node to create a connection
			listen(ReceiveSocket)
		end.

broadcast() ->
	{ok, SendSocket} = gen_udp:open(?SEND_PORT, [list, {active, true}, {broadcast, true}]),
	broadcast(SendSocket).

broadcast(SendSocket) ->
	ok = gen_udp:send(SendSocket, {255,255,255,255}, ?RECEIVE_PORT, atom_to_list(node())),
	timer:sleep(2000),
	broadcast(SendSocket).
