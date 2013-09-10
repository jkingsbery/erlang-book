-module(frequency).
-export([start/0, stop/0, allocate/0, deallocate/1]).
-export([init/0]).


% Server
start()->
    register(frequency,spawn(frequency,init,[])).

init()->
    Frequencies={get_frequencies(),[]},
    loop(Frequencies).

loop(Frequencies)->
    receive
	{request,Pid,allocate}->
	    {NewFrequencies,Reply}=allocate(Frequencies,Pid),
	    reply(Pid,Reply),
	    loop(NewFrequencies);
	{request,Pid,{deallocate,Freq}} ->
	    NewFrequencies = deallocate(Frequencies, Freq),
	    reply(Pid,ok),
	    loop(NewFrequencies);
	{request,Pid,stop} ->
	    reply(Pid,ok),
	    true
    end.

reply(Pid,Reply) ->
    Pid ! {reply, Reply}.

%allocate(Frequencies,Pid)
allocate({[],InUseFrequences},_Pid)->
    {{[],InUseFrequences},{error,no_frequency_available}};
allocate({[Freq|OtherFrequencies],InUseFrequences},Pid) ->
    {{OtherFrequencies,[{Freq,Pid}|InUseFrequences]},{ok,Freq}}.

deallocate({Free,Allocated},Freq)->
    NewAllocated=lists:keydelete(Freq,1,Allocated),
    {[Freq|Free],NewAllocated}.
    


get_frequencies()->
    [10,11,12,13,14,15].


%Client
stop()->
    call(stop).
allocate()->
    call(allocate).
deallocate(Freq)->
    call({deallocate,Freq}).

call(Message)->
    frequency ! {request,self(),Message},
    receive
	{reply, Reply} ->
	     Reply
    end.
