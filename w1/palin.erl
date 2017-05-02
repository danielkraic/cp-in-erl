-module(palin).
-export([palin/1,nopunct/1,palindrome/1, server/0, client/1]).

% palindrome problem
%
% palindrome("Madam I\'m Adam.") = true

palindrome(Xs) ->
    palin(nocaps(nopunct(Xs))).

nopunct([]) ->
    [];
nopunct([X|Xs]) ->
    case lists:member(X,".,\ ;:\t\n\'\"") of
	true ->
	    nopunct(Xs);
	false ->
	    [ X | nopunct(Xs) ]
    end.

nocaps([]) ->
    [];
nocaps([X|Xs]) ->
    [ nocap(X) | nocaps(Xs) ].

nocap(X) ->
    case $A =< X andalso X =< $Z of
	true ->
	    X+32;
	false ->
	    X
    end.

% literal palindrome

palin(Xs) ->
    Xs == reverse(Xs).

reverse(Xs) ->
    shunt(Xs,[]).

shunt([],Ys) ->
    Ys;
shunt([X|Xs],Ys) ->
    shunt(Xs,[X|Ys]).


server() ->
    receive
        stop ->
            io:format("server stopped~n");
        {check, ResultPid, Msg} ->
            io:format("received msg: ~s~n", [Msg]),
            case palindrome(Msg) of
                true ->
                    Result = io_lib:format("~s is a palindrome", [Msg]),
                    io:format("~s~n", [Result]),
                    ResultPid ! {result, Result};
                false ->
                    Result = io_lib:format("~s is NOT a palindrome", [Msg]),
                    io:format("~s~n", [Result]),
                    ResultPid ! {result, Result}
            end,
            server()
    end.

client(ClientName) ->
    receive
        stop ->
            io:format("client ~s stopped~n", [ClientName]);
        {result, Result} ->
            io:format("client ~s received msg: ~s~n", [ClientName, Result]),
            client(ClientName)
    end.