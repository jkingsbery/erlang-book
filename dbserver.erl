-module(dbserver).

start()->
    register(dbserver,spawn(dbserver,loop,db:new())),
    ok.

loop(Db)->
    receive
	{From,stop}->
	    true;
	{From,_} ->
	    From ! {self(), no_such_command},
	    loop(Db)
    end.

stop()->
    ok.
write(Key,Element)->
    ok.
delete(Key)->
    ok.
read(Key)->
    {ok,element}. % or {error, instance}.
match(Element)->
    [].
