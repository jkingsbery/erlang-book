-module(dbserver).

-export([start/0,stop/0,write/2,delete/1,read/1,match/1,loop/1]).

start()->
    register(db, spawn(?MODULE,loop,[db:new()])).


stop()->
    db ! stop.

loop(Db)->
    receive
	{write, From, Key, Element }->
	    NewDb=db:write(Key,Element,Db),
	    From ! {self(), ok},
	    loop(NewDb);
	{read, From, Key}->
	    From ! {self(), db:read(Key,Db)},
	    loop(Db);
	{match, From, Element}->
	    From ! {self(),db:match(Element,Db)},
	    loop(Db);
	{delete,From,Element}->
	    NewDb=db:delete(Element,Db),
	    From ! {self(),ok},
	    loop(NewDb);
	
	stop ->
	    true
    end.

write(Key,Element)->
    db ! {write,self(),Key,Element},
    receive
	{_Pid,ok}->
	    ok;
	_ ->
	    false
    end.

delete(Key)->
    db ! {delete,self(),Key},
    receive
	{_Pid,Response}->
	    Response
    end.

read(Key)->
    db ! {read,self(),Key},
    receive
	{_Pid,Response}->
	    Response
    end.
	
match(Element)->
    db ! {match,self(),Element},
    receive
	{_Pid,Response}->
	    Response
    end.

