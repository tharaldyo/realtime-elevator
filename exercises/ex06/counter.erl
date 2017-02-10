-module(counter).
-compile(export_all).

start() ->
	Counter = spawn(counter, count, [0]).

count(Number) ->
	Backup = spawn(counter, backup, [Number]),
	io:format("~p~n", [self()]), %TODO: fix this
	loop(Number, Backup).
	% create backup:
	% 	Backup = spawn(counter, mode_backup).
	% [optional] loop until backup replies
	% loop: count + send count to backup

loop(Number, Backup) ->
	if
		Number == 5 ->
			exit(kill)
	end,
			
	timer:sleep(1000),
	Backup ! Number,
	io:format("~p~n", [Number]),
	loop(Number+1, Backup).

backup(Number) ->
	io:format("temp ~n"),
	
	b_loop(Number),
	io:format("hello from under b_loop call ~n"),
	count(Number+1).
	
	
b_loop(Nu) ->
	Num = receive Number ->
			     io:format("backup received: ~p~n", [Number]),
			     b_loop(Nu)
	      after 3000 ->
			    0
	      end,
	io:format("b_loop reached after 3000 ~n").
	
