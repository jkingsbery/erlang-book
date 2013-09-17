-module(dbserver2).

-export([start_link/0,stop/0,init/1,terminate/2]).
-export([handle_cast/2,handle_call/3]).
-export([write/2,read/1,delete/1,match/1]).


-behavior(gen_server).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE,[],[]).

init(_Args) ->
    {ok,db:new() }.

handle_cast(stop,LoopData) ->
    {stop, normal, LoopData}.

handle_call({write,Key,Element},_From,LoopData) ->
    {reply,ok,db:write(Key,Element,LoopData)};
handle_call({read,Key},_From,LoopData) ->
    case db:read(Key,LoopData) of
        {ok,Element} ->
            {reply,Element,LoopData};
        {error,_} ->
            {reply,error,LoopData}
    end;
handle_call({match,Element},_From,LoopData) ->
    {reply,db:match(Element,LoopData),LoopData};
handle_call({delete,Key},_From,LoopData) ->
    {reply,ok,db:delete(Key,LoopData)}.
            
    

terminate(_Reason, _LoopData)->
    ok.

% Client API
stop()->
    gen_server:cast(?MODULE, stop).
write(Key,Element)->
    gen_server:call(?MODULE,{write,Key,Element}).
read(Key)->
    gen_server:call(?MODULE,{read,Key}).
delete(Key)->
    gen_server:call(?MODULE,{delete,Key}).
match(Element)->
    gen_server:call(?MODULE,{match,Element}).
