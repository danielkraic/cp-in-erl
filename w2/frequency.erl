%% Based on code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).
-export([start/0,allocate/0,deallocate/1,stop/0,client/1]).
-export([init/0]).

%% These are the start functions used to create and
%% initialize the server.

start() ->
  case whereis(frequency)
    undefined ->
      register(frequency, spawn(frequency, init, []));
    _Pid ->
      server_already_started
  end.

init() ->
  process_flag(trap_exit, true),
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop

loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      NewFrequencies = deallocate(Frequencies, Freq),
      Pid ! {reply, ok},
      loop(NewFrequencies);
    {request, Pid, stop} ->
      Pid ! {reply, stopped};
    {'EXIT', Pid, _Reason} ->
      NewFrequencies = exited(Frequencies, Pid),
      loop(NewFrequencies)
  end.

%% Functional interface

allocate() ->
    frequency ! {request, self(), allocate},
    receive
	    {reply, Reply} -> Reply
    end.

deallocate(Freq) ->
    frequency ! {request, self(), {deallocate, Freq}},
    receive
	    {reply, Reply} -> Reply
    end.

stop() ->
    frequency ! {request, self(), stop},
    receive
	    {reply, Reply} -> Reply
    end.

% client functionality

start_client(Name) ->
    client_allocate(Name).

client_allocate(Name) ->
  timer:sleep(1000),
  case is_server_down() of
    true ->
      io:format("server is down. stopping client ~s~n", [Name]);
    false ->
      case allocate() of
          {ok, Freq} ->
              io:format("client ~s allocated ~w~n", [Name, Freq]),
              client_deallocate(Name, Freq);
          {error, no_frequecny} ->
              io:format("client ~s NOT allocated. no frequency~n", [Name]),
              client_allocate(Name)
      end
  end.

client_deallocate(Name, Freq) ->
  timer:sleep(1000),
  case is_server_down() of
    true ->
      io:format("server is down. stopping client ~s~n", [Name]);
    false ->
      deallocate(Freq),
      io:format("client ~s deallocated ~w~n", [Name, Freq]),
      client_allocate(Name)
  end.

is_server_down() ->
  Msgs = get_exit_msgs([]),
  case length(Msgs) > 0 of
    true ->
      true;
    false ->
      false
  end.

get_exit_msgs(ExitMsgs) ->
  receive
    {'EXIT', Pid, Reason} ->
      get_exit_msgs([{'EXIT', Pid, Reason}|ExitMsgs]);
    _Msg ->
      % skipping all other messages
      get_exit_msgs(ExitMsgs)
  after 0 ->
    % if there are no message in mailbox
    ExitMsgs
  end.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  link(Pid),
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
  {value,{Freq,Pid}} = lists:keysearch(Freq,1,Allocated),
  unlink(Pid),
  NewAllocated=lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free],  NewAllocated}.

exited({Free, Allocated}, Pid) ->
    io:format("client ~s exited. ~n", [Pid]),
    case lists:keysearch(Pid,2,Allocated) of
      {value,{Freq,Pid}} ->
        io:format("client ~s exited. frequency deallocated~n", [Pid]),
        NewAllocated = lists:keydelete(Freq,1,Allocated),
        {[Freq|Free],NewAllocated};
      false ->
        io:format("client ~s exited. frequency not found~n", [Pid]),
        {Free,Allocated}
    end.
