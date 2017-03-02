-module(network).
-export([start/0]).

start() ->
  {ok, Socket} = gen_udp:open(20020, [list, {active, false}]),
  inet:setopts(Socket, [{active, true}]),
  io:format("socket opened: ~p ~n", [Socket]), 
  ok = gen_udp:send(Socket, "129.241.187.43", 20020, <<"hello 20">>),
  receive{udp, Socket, Host, Port, Bin} ->
           io:format("message received: ~p ~n", [string:strip(Bin, right, 0)])

  end.


% Loop!
%  loop(Socket).

%loop(Socket) ->
%  receive{udp, Socket, Host, Port, Bin} ->
%           io:format("message received: ~p ~n", [Bin]),
%           ok = gen_udp:send(Socket, "129.241.187.43", 20020, <<"hello 20">>),
%  end,
%  loop(Socket).

%gen_udp:close(Socket).
