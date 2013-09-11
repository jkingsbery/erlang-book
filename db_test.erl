-module(db_test).

-include_lib("eunit/include/eunit.hrl").

write1_test()->
    Db1=db:new(),
    Db3=db:write(86753,katie,Db1),
    {ok,katie}=db:read(86753,Db3).

empty_db_test()->
    {db,[]}=db:new().
    

not_there_test()->
    Db1=db:new(),
    {error,instance}=db:read(86753,Db1).

book_test()->
    Db=db:new(),
    Db1=db:write(francesco,london,Db),
    Db2=db:write(lelle,stockholm,Db1),
    {ok,london}=db:read(francesco,Db2),
    Db3=db:write(joern,stockholm,Db2),
    {error,instance}=db:read(ola,Db3),
    [joern,lelle]=db:match(stockholm,Db3),
    Db4=db:delete(lelle, Db3),
    [joern]=db:match(stockholm,Db4).
