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
	io:format("I'm crashing :-( ~n"),
	exit(normal).

backup(Number) ->
	io:format("New backup created. ~n"),
	b_loop(Number).
	
b_loop(Number) ->
	receive N ->
		b_loop(N)
	after 2000 ->
		count(Number+1)
	end.
