-module(db).
-export([new/0,destroy/1,write/3,delete/2,read/2,match/2]).

new()->
    {db,[]}.

destroy(Db)->
    {ok}.

write(Key,Element,{db,List})->
    {db,[{Key,Element}|List]}.


%delete(Key,Db)->
delete(Key,{db,[]})->
    {db,[]};
delete(Key,{db,[{Key,Element}|List]})->
    {db,List};
delete(Key,{db,[{_OtherKey,_OtherElement}|List]})->
    [{_OtherKey,_OtherElement}|delete(Key,{db,List})].
    

read(_Key,{db,[]})->
    {error,instance};
read(_Key,{db,[{_Key,Element}|Db]})->
    {ok,Element};
read(Key,{db,[{_,_}|Db]})->
    read(Key,{db,Db}).

%match(Element,Db)->
match(Element,{db,[]})->
    [];
match(Element,{db,[{Key,Element}|Db]})->
    [Key|match(Element,{db,Db})];
match(Element,{db,[{_,_}|List]})->
    match(Element,{db,List}).
