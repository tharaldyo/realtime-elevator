-module(elevator).
-export([start/0]).

start() ->
	elev_driver:start(self(), elevator).
