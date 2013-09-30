-module(evserv).
-compile(export_all).

-record(state,{events,clients}).

-record(event,{name="",description="",pid,timeout=20*60*60}).

init()->
    loop(#state{events=orddict:new(),clients=orddict:new()}).

start() ->
    register(?MODULE, Pid=spawn(?MODULE, init, [])),
    Pid.

start_link() ->
    register(?MODULE, Pid=spawn_link(?MODULE, init, [])),
    Pid.

termiante() ->
    ?MODULE ! shutdown.

subscribe(Pid) ->
    Ref = erlang:monitor(process, whereis(?MODULE)),
    ?MODULE ! {self(), Ref, {subscribe, Pid}},
    receive
        {Ref, ok} ->
            {ok, Ref};
        {'DOWN', Ref, process, _Pid, Reason} ->
            {ereror, Reason}
    after 5000->
            {error, timeout}
    end.

add_event(Name, Description, Timeout)->
    Ref=make_ref(),
    ?MODULE ! {self(), Ref, {add, Name, Description, Timeout}},
    receive
        {Ref, Msg} ->
            Msg
    after 5000 ->
            {error, timeout}
    end.

cancel(Name) ->
    Ref=make_ref(),
    ?MODULE! {self(),Ref, {cancel, Name}},
    receive
        {Ref, ok} ->
             ok
    after 5000 ->
            {error, timeout}
    end.

listen(Delay) ->
    receive
        M={done, _Name, _Description} ->
            [M | listen(0)]
    after Delay*1000->
            []
    end.
                        


valid_datetime(_Timeout)->
    true.

loop(State=#state{}) ->
    receive
        {Pid,MsgRef,{subscribe, Client}} ->
            Ref=erlang:monitor(process, Client),
            NewClients=orddict:store(Ref,Client,State#state.clients),
            Pid ! {MsgRef, ok},
            loop(State#state{clients=NewClients});
        {Pid,MsgRef,{add,Name,Description,Timeout}} ->
            case valid_datetime(Timeout) of
                true ->
                    EventPid = event:start_link(Name,Timeout),
                    Event=#event{name=Name,description=Description,pid=EventPid,timeout=Timeout},
                    NewEvents=orddict:store(Name,Event,State#state.events),
                    Pid ! {MsgRef, ok},
                    loop(State#state{events=NewEvents});
                false ->
                    Pid ! {MsgRef, {error, bad_timeout}},
                    loop(State)
            end;
        {Pid,MsgRef,{cancel,Name}} ->
            Events=case orddict:find(Name,State#state.events) of
                       {ok,E}->
                           event:cancel(E#event.pid),
                           orddict:erase(Name,State#state.events);
                       error->
                           State#state.events
                   end,
            Pid ! {MsgRef, ok},
            loop(State#state{events=Events});
        {done,Name} ->
            case orddict:find(Name, State#state.events) of
                {ok, E} ->
                    send_to_clients({done, E#event.name, E#event.description},State#state.clients),
                    NewEvents = orddict:erase(Name, State#state.events),
                    loop(State#state{events=NewEvents});
                error ->
                    loop(State)
            end;
        shutdown ->
            exit(shutdown);
        {'DOWN',Ref,process, _Pid, _Reason} ->
            loop(State#state{clients=orddict:erase(Ref, State#state.clients)});
        code_change ->
            ?MODULE:loop(State);
        Unknown ->
            io:format("Unknown message: ~p~n",[Unknown]),
            loop(State)
    end.

send_to_clients(Msg, ClientDict) ->
    SendFun=fun(_Ref, Pid) -> Pid ! Msg end,
    orddict:map(SendFun, ClientDict).
