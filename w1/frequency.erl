%% Based on code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).
-export([start/0,allocate/0,deallocate/1,stop/0,clear/0]).
-export([init/0]).

%% These are the start functions used to create and
%% initialize the server.

start() ->
    register(frequency,
       spawn(frequency, init, [])).

init() ->
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop

loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      % timer:sleep(1000),
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      % timer:sleep(1000),
      {NewFrequencies, Reply} = deallocate(Frequencies, Freq),
      Pid ! {reply, Reply},
      loop(NewFrequencies);
    {request, Pid, stop} ->
      Pid ! {reply, stopped}
  end.

%% Functional interface

allocate() ->
    clear(),
    frequency ! {request, self(), allocate},
    receive
      {reply, Reply} -> Reply
    after 1000 ->
      {error, timeout}
    end.

deallocate(Freq) ->
    clear(),
    frequency ! {request, self(), {deallocate, Freq}},
    receive
      {reply, Reply} -> Reply
    after 1000 ->
      {error, timeout}
    end.

stop() ->
    clear(),
    frequency ! {request, self(), stop},
    receive
      {reply, Reply} -> Reply
    end.

clear() ->
  receive
    {reply, Reply} ->
      io:format("mailbox msg cleared: ~p~n", [Reply]),
      clear()
  after 0 ->
    % terminate if there is no message in mailbox
    ok
  end.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[Freq|Free], Allocated}, Pid) ->
  AlreadyAllocated = lists:any(fun({_ElFreq, ElPid}) -> ElPid =:= Pid end, Allocated),
  case AlreadyAllocated of
    true ->
      {{[Freq|Free], Allocated}, {error, already_allocated}};
    false ->
      {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}
  end.

deallocate({Free, Allocated}, Freq) ->
  AlreadyAllocated = lists:any(fun({ElFreq, _ElPid}) -> ElFreq =:= Freq end, Allocated),
  case AlreadyAllocated of
    true ->
      NewAllocated = lists:keydelete(Freq, 1, Allocated),
      {{[Freq|Free], NewAllocated}, ok};
    false ->
      {{Free,  Allocated}, {error, not_found}}
  end.
