-module(chap3).

-export([create/1,create/2,print_list/1,print_nums/1]).

%create(1) ->
%    [1];
%create(N) -> 
%    [N|create(N-1)].

create(N) ->
    create(N,N).


create(X,Max)->
    if
	X==0 -> [];
	true -> [Max - X+1 | create(X-1,Max)]
    end.


print_nums(N)->
    print_list(create(N)).

print_list([])->
    {ok};
print_list([H|T])->
    io:format("Number:~p~n",[H]),
    print_list(T).
