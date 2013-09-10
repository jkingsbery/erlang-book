-module(echoclient).

-export([go/0]).

go()->
    echoserver:start(),
    echoserver:print(hello),
    echoserver:print(foo),
    echoserver:print(bar),
    echoserver:print(3),
    echoserver:print("what's up?"),
    echoserver:stop().
