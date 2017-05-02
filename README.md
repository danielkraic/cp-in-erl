# cp-in-erl

notes from online course Concurrent Programming in Erlang

## Message-passing concurrency

### erlang

* concurrent
* high-availability
* fault-tolerant

* functional programming:
  * simple values
  * high-level patterns (map/reduce)
  * immutability (thread-safe, safe caching and sharing, consistency)

### Message-passing concurrency

* different processes share nothing
* different processes communicate by passing messages


### Actor model

* everything is actor
* actors communicate by messages
* actors can create other actors
* actors can change behaviour according to messages

### Processes

* exist in Erlang VM (not in OS)
* share nothing
* more lightweight than threads
* more numerous

* running in its own space
* time-sharing with other processes
* runs in erlang function

* *spawn* - create process
* *!* - send message
* *self()* - get pid
* *receive* - handle message

```erlang
% create process and return its pid
Pid = spawn(Mod, Fun, Args)
```

```erlang
% send message to mailbox of process Pid
Pid ! Msg
Name ! Msg

% message send to non-existent PID will dissappear
% message send to non-existent Name is an error

% flush mailbox in erlang shell
flush().
```

```erlang
% get process Pid
self()
```

```erlang
receive
  stop ->
    ... terminate ...
  Msg ->
    ... handle msg ...
    % then call same function again to process another message
After 500 ->
  % after 500 ms
  ...
end
```

```erlang
Pid = spawn(...)
register(Name, Pid)

Pid = whereis(Name)
```

```erlang
% flush mailbox
clear() ->
  receive
    _Msg -> clear()
  after 0 ->
    % if there is  no message in mailbox
    ok
  end.
```

## Abstract patterns

```erlang
for(Max,Max,F) ->
  [F(Max)];
for(I,Max,F) ->
  [F(I)|for(I+1,Max,F)].

for(1,5,fun(I) -> I * I end).
```

```erlang
rpc(Pid, Request) ->
  Tag = erlang:make_ref(),
  Pid ! {self(), Tag, Request},
  receive
    {Tag, Response} ->
      Response
    after <Time> ->
      ...
  end.
```

```erlang
% rpc split in two

% rpc
promise(Pid, Request) ->
  Tag = erlang:make_ref(),
  Pid ! {self(), Tag, Request},
  Tag.

% wait for response
yield(Tag) ->
  receive
    {Tag, Response} ->
      Response
  end.

Tag = promise(Pid, fun() -> ... end),
...
Val = yield(Tag)
```

```erlang
function pmap(L) ->
  S = self(),
  Pids = [do(S,F) || F <- L],
  [receive {Pid,Val} -> Val end || Pid <- Pids].

do(Parent, F) ->
  spawn(fun() ->
      Parent ! {self(), F()}
    end).
```

```erlang
-module(calc).
-export([start/0, stop/0, execute/1]).

start() -> spawn(calc, init, []).

init() ->
  io:format("starting calc...~n"),
  register(calc, self()),
  loop().

loop() ->
  receive
    {request, From, Expr} ->
      From ! {reply, expr:eval(Expr)},
      loop();
    stop() ->
      io:format("Terminating...~n")
    end.

stop() ->
  calc ! stop.

execute(X) ->
  calc ! {request, self(), X},
  receive
    {reply, Reply} ->
      Reply
  end.
```

## Building robust systems

### Links, signals and messages

* link define error propagation path
* when process terminates abnormally, it send a signal to all the processes linked to it, with reason _killed_
* after receiving signal, process terminate by default

```erlang
link(Pid)
unlink(Pid)
```

```erlang
P = spawn_link(?MODULE, loop, [])
```

```erlang
% exit signals to that process will be converted to messages
process_flag(trap_exit, true)

% to turn it off: process_flag(trap_exit, false)

receive
  {'EXIT', FromPid, Reason}
```

```erlang
% call exit signal explicitly
exit(Reason)

exit(Pid, normal)  % normal exit: do noting or receives {'EXIT', Pid, normal}
exit(Pid, Reason)  % abnormal exit: terminates abnormally OR receives {'EXIT', Pid, Reason}
exit(Pid, kill)    % kill (override exit trapping) process terminates abnormally (but not kill other linked processes)
```

### Supervisors

* monitor process failures and restart process
* link: symetrical linkage between processes
* monitor: asymetrical linkage between processes
