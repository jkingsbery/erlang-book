-module(chap3).

-export([create/1,create/2]).

create(1) ->
    [1];
create(N) -> 
    [N|create(N-1)].

create(X,Max)->
    if
	X==0 -> [];
	true -> [Max - X+1 | create(X-1,Max)]
    end.

