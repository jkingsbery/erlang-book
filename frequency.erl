-module(frequency).
-export([start/0, stop/0, allocate/0, deallocate/1,available/0]).
-export([init/0]).

-export([allocate/2,deallocate/3]).

% TODO: Only the process that allocates should be able to deallocate
% TODO: Processes should have a maximum number of frequencies allocated

% Server
start()->
    case whereis(frequency) of
	undefined ->
	    register(frequency,spawn(frequency,init,[]));
	_Else ->
	    {error,already_started}
    end.

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
	    {Status,NewFrequencies} = deallocate(Frequencies, Freq,Pid),
	    reply(Pid,Status),
	    loop(NewFrequencies);
	{request,Pid,available}->
	    {Free,_InUse}=Frequencies,
	    reply(Pid,Free),
	    loop(Frequencies);
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

deallocate({Free,Allocated},Freq,Requester)->
    IsFree=lists:member(Freq,Free),
    AllocatedBy=lists:keyfind(Freq,1,Free),
    io:format("~b~n",[Requester]),
    io:format("~b~n",[AllocatedBy]),
    if  
	not IsFree and AllocatedBy=:=Requester->
	    NewAllocated=lists:keydelete(Freq,1,Allocated),
	    {ok,{[Freq|Free],NewAllocated}};
        IsFree ->
            {error_already_free,{Free,Allocated}};
        AllocatedBy=/=Requester ->
            {error_not_allocated_by_requester,{Free,Allocated}};
	true ->
	    {error,{Free,Allocated}}
    end.


get_frequencies()->
    [10,11,12,13,14,15].


%Client
stop()->
    call(stop).
allocate()->
    call(allocate).
deallocate(Freq)->
    call({deallocate,Freq}).
available()->
    call(available).

call(Message)->
    case whereis(frequency) of
	undefined ->
	    {error,frequency_server_unavailable}; 
	_Else ->
	    frequency ! {request,self(),Message},
	    receive
		{reply, Reply} ->
		    Reply
	    end
    end.
