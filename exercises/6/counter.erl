-module(counter).
-compile(export_all).

start() ->
	Counter = spawn(counter, count, [0]).

count(Number) ->
	Backup = spawn(counter, backup, [Number]),
	io:format("~p~n", [self()]),
	loop(Number, Backup, 1).

loop(Number, Backup, Rand) when 3 /= Rand ->
	timer:sleep(1000),
	Backup ! Number,
	io:format("~p~n", [Number]),
	RandNew = rand:uniform(5),
	loop(Number+1, Backup, RandNew);
loop(Number, Backup, Rand) when 3 == Rand ->
	exit(normal).

backup(Number) ->
	io:format("temp ~n"),
	b_loop(Number).
	
b_loop(Number) ->
	receive N ->
		b_loop(N)
	after 5000 ->
		count(Number)
	end.
