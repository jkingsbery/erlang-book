-module(echoserver).

-export([start/0,loop/0,print/1,stop/0]).

% Server: waits for messages, and either prints its contents or terminates
% Client: sends messages
% Functional interface provided to client

start()->
    register(echo, spawn(echoserver,loop,[])).

print(Term)->
    echo ! {self(),Term},
    receive
	{_Pid, Msg}->
	    io:format("~w~n",[Msg])
    end,
    ok.

stop()->
    echo ! stop.

loop()->
    receive
	{From, Msg}->
	    From ! {self(), Msg},
	    loop();
	stop ->
	    true
    end.
