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


### Exceptions

* *let it fail!*

```erlang
area(H,W) when H>0, W>0 ->
  {ok,H*W};
area(H,W) ->
  {error, negative_args}.
```

```erlang
eval(Env, {div,Num,Denom}) ->
  N = eval(Env, Num),
  D = eval(Env, Denom),
  case D of
    0 ->
      throw(div_by_zero);
    _NZ ->
      N div D
  end;

try eval (Env, Exp) of
  Res ->
    {ok, Res}
catch
  throw:div_by_zero ->
    {error, div_by_zero}
end
```

```erlang
try eval(Env, Exp) of
catch
  throw:_ ->
    _
  error:_ ->
    _
  exit:_ ->, distribution
    _
end
```

#### Errors

* badmatch
* badarg
* badarith
* undef
* function\_clause
* if\_clause
* case\_clause

### Erlang tools

* erlang observer
* percept2
* conqueror
* quickcheck + pulse

### Hot code loading

#### Loading code into BEAM

* when function from Foo is called
* _c(Foo)_ or _compile:file(Foo)_
* _code:load\_file(Foo)_
* _code:is\_loaded(Foo)_

#### Using new code

* after new code of Foo module is loaded, module using Foo is still using same code, until there is a call to any function in Foo, when it switches

#### Purge old code

* _code:purge(Foo)_ (always succeeds - may cause failure)
* _code:soft_purge(Foo)_ (succeeds only if old version of code is not used)

## Scaling it up

### Multicore Erlang

* each core has a separate run-queue
* scheduling is per-core
* processes spawned on the core that spawns them
* processes move between cores (work stealing)
* garbage collection is per-process

### Distributed Erlang

* multiple instances of Erlang running on different hosts
* communicating by message passing to Pids and named processes
* security throught a shared secret ("cookie")

#### Named node

* short-named nodes communicate over local network
```bash
erl -sname ant -setcookie bee
```
* long-named nodes communicate globally
  * _ant@192.168.11.121_ or _ant@cs.kent.ac.uk_
* send message
```erlang
{elf, 'ant@baz'} ! Message
```
* spawn process on another node (code must be compiled on remote node)
```erlang
Pid = spawn('ant@baz', M,F,A)
```
* change cookie _erlang:set_cookie_
* nodes info: _node/0_ and _nodes/0_

### Bulk storage

* tuple based table storage:
  * Erlang Term Storage ETS http://erlang.org/doc/man/ets.html
  * persistent file-based DETS http://erlang.org/doc/man/dets.html

## OTP

* libraries (protocols, web server,...)
* design patterns (server, event handler, supervisor,...)
* generic behaviours

```erlang
-module(gecho).
-behaviour(gen_server).
-compile(export_all).

start_link() ->
    gen_server:start_link(
		{local, ?MODULE}, 
		?MODULE, [], []).

count() ->
    gen_server:call(?MODULE,count).

echo(X) ->
    gen_server:call(?MODULE,X).

reset(N) ->
    gen_server:cast(?MODULE,{reset,N}).

stop() ->
    gen_server:stop(?MODULE).


init([]) ->
    {ok, 0}.

handle_call(count, _From, State) ->
    {reply, State, State};

handle_call(Msg, _From, State) ->
    {reply, Msg, State+1}.


handle_cast({reset,N}, _State) ->
    {noreply, N}.
```

## Records

```erlang
%% recs.hrl
-record(twitter, {name, handle}).
```

```erlang
-include("recs.hrl").

r1() -> #twitter{name="simon", handle="@si"}.

get_name(X) -> X#twitter.name.

update_si(#twitter{name="simon"} = X) ->
  X#twitter{name="si"};
update_si(X) ->
  X.
```

## Maps
```erlang
map1() -> #{name => "simon", handle => "@si"}.

get_name(#{name := Y}) -> Y.

update_si(#{name := "simon"} = X) ->
  X#{name := "si"};
update_si(X) ->
  X.
```

## Tools

* _observer:start()_ processes, applications, tracing
* typer and dialyzer
* wrangler - refactoring
* QuickCheck and PropEr, Concuerror, McErlang, Cuter
