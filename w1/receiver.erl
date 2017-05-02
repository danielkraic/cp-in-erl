-module(receiver).
-export([rec/0, rec2/0]).

rec() ->
    % timer:sleep(M)
    receive
        stop ->
            io:format("receiver stopped~n");
        Msg ->
            io:format("received msg: ~s~n", [Msg]),
            rec()
    end.

rec2() ->
    Msgs = orddict:new(),
    rec_msgs(orddict:size(Msgs), Msgs).

rec_msgs(2, Msgs) ->
    io:format("receiver has 2 messages~n"),
    io:format("first: ~s~n", [orddict:fetch(first)]),
    io:format("second: ~s~n", [orddict:fetch(second)]),
    rec2();
rec_msgs(_, Msgs) ->
    receive
        stop ->
            io:format("receiver stopped~n");
        {first, First} ->
            io:format("received first msg: ~n"),
            UpdMsgs = orddict:store(first, First, Msgs),
            rec_msgs(orddict:size(UpdMsgs), UpdMsgs);
        {second, Second} ->
            io:format("received second msg: ~n"),
            UpdMsgs = orddict:store(second, Second, Msgs),
            rec_msgs(orddict:size(UpdMsgs), UpdMsgs)
    end.
