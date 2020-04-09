-module(day_23).

%% API exports
-export([main/1, run/1, read/2, write/5, shift_offset/2, get_param/4, get_mode/3, spawn_computers/2, count_messages/2]).

%%====================================================================
%% API functions
%%====================================================================
-record(computer, {mem, pointer = 0, relative_base = 0, pid = -1}).

read(Mem, Addr) ->
  maps:get(Addr, Mem, 0).

shift_offset(Value, 0) -> Value;
shift_offset(Value, N) when N > 0 ->
  shift_offset(Value div 10, N - 1).

get_mode(Mem, Pointer, Offset) ->
  Value = read(Mem, Pointer) div 100,
  shift_offset(Value, Offset - 1) rem 10.


write(Mem, Pointer, RelativeBase, Offset, Value) ->
  Mode = get_mode(Mem, Pointer, Offset),
  Addr = case Mode of
           0 -> read(Mem, Pointer + Offset);
           1 -> Pointer + Offset;
           2 -> read(Mem, Pointer + Offset) + RelativeBase
         end,
  Updated = maps:put(Addr, Value, Mem),
  Updated.

get_param(Mem, Pointer, RelativeBase, Offset) ->
  Mode = get_mode(Mem, Pointer, Offset),
  Addr = case Mode of
           0 -> read(Mem, Pointer + Offset);
           1 -> Pointer + Offset;
           2 -> read(Mem, Pointer + Offset) + RelativeBase
         end,
  read(Mem, Addr).

add(State) ->
  A = get_param(State#computer.mem, State#computer.pointer, State#computer.relative_base, 1),
  B = get_param(State#computer.mem, State#computer.pointer, State#computer.relative_base, 2),
  NextMem = write(State#computer.mem, State#computer.pointer, State#computer.relative_base, 3, A + B),
  NextState = State#computer{mem = NextMem, pointer = State#computer.pointer + 4},
  run(NextState).

mul(State) ->
  A = get_param(State#computer.mem, State#computer.pointer, State#computer.relative_base, 1),
  B = get_param(State#computer.mem, State#computer.pointer, State#computer.relative_base, 2),
  NextMem = write(State#computer.mem, State#computer.pointer, State#computer.relative_base, 3, A * B),
  NextState = State#computer{mem = NextMem, pointer = State#computer.pointer + 4},
  run(NextState).

in(State) ->
%%  {message_queue_len, Len} = process_info(self(), message_queue_len),

%%  if
%%     Len == 0 ->
%%       master ! {op, Len,777},
%%       NextMem = write(State#computer.mem, State#computer.pointer, State#computer.relative_base, 1, 1),
%%       NextState = State#computer{mem = NextMem, pointer = State#computer.pointer + 2},
%%       run(NextState);
%%    true ->
  master ! {input, self()},
  receive
    X ->
      master ! thx,
      NextMem = write(State#computer.mem, State#computer.pointer, State#computer.relative_base, 1, X),
      NextState = State#computer{mem = NextMem, pointer = State#computer.pointer + 2},
      run(NextState)
  end,
%%  end,
  master ! {thx, State#computer.pid}.
%%  NextState = case State#computer.status of
%%                booting -> % no pid assigned
%%                  NextMem = write(State#computer.mem, State#computer.pointer, State#computer.relative_base, 1, State#computer.next_val),
%%                  State#computer{mem = NextMem};
%%                ready -> % next value ready, pid assigned
%%                  NextMem = write(State#computer.mem, State#computer.pointer, State#computer.relative_base, 1, State#computer.next_val),
%%                  State#computer{mem = NextMem, status = waiting};
%%                waiting -> % waiting for message
%%                  {message_queue_len, Len} = process_info(self(), message_queue_len),
%%                  if
%%                    Len == 0 ->
%%                      NextMem = write(State#computer.mem, State#computer.pointer, State#computer.relative_base, 1, -1),
%%                      State#computer{mem = NextMem, status = waiting};
%%                    true ->
%%                      receive
%%                        {X, Y} ->
%%                          NextMem = write(State#computer.mem, State#computer.pointer, State#computer.relative_base, 1, X),
%%                          State#computer{mem = NextMem, next_val = Y, status = ready}
%%                      end
%%                  end
%%              end,
%%  run(NextState).

out(State) ->
  A = get_param(State#computer.mem, State#computer.pointer, State#computer.relative_base, 1),
  master ! {output, self(), A},
  NextState = State#computer{pointer = State#computer.pointer + 2},
  run(NextState).
%%  NextState = case State#computer.recv_stat of
%%                empty ->
%%                  State#computer{send_addr = A, recv_stat = add_ready, pointer = State#computer.pointer + 2};
%%                addr_ready ->
%%                  State#computer{send_val1 = A, recv_stat = first_ready, pointer = State#computer.pointer + 2};
%%                first_ready ->
%%                  master ! {self(), State#computer.send_addr, State#computer.send_val1, A},
%%                  State#computer{recv_stat = empty, pointer = State#computer.pointer + 2}
%%              end,
%%  run(NextState).


jmpnz(State) ->
  A = get_param(State#computer.mem, State#computer.pointer, State#computer.relative_base, 1),
  B = get_param(State#computer.mem, State#computer.pointer, State#computer.relative_base, 2),
  Pointer = if
              A /= 0 -> B;
              true -> State#computer.pointer + 3
            end,
  NextState = State#computer{pointer = Pointer},
  run(NextState).

jmpz(State) ->
  A = get_param(State#computer.mem, State#computer.pointer, State#computer.relative_base, 1),
  B = get_param(State#computer.mem, State#computer.pointer, State#computer.relative_base, 2),
  Pointer = if
              A == 0 -> B;
              true -> State#computer.pointer + 3
            end,
  NextState = State#computer{pointer = Pointer},
  run(NextState).

lt(State) ->
  A = get_param(State#computer.mem, State#computer.pointer, State#computer.relative_base, 1),
  B = get_param(State#computer.mem, State#computer.pointer, State#computer.relative_base, 2),
  Value = if
            A < B -> 1;
            true -> 0
          end,
  NextMem = write(State#computer.mem, State#computer.pointer, State#computer.relative_base, 3, Value),
  NextState = State#computer{mem = NextMem, pointer = State#computer.pointer + 4},
  run(NextState).


eq(State) ->
  A = get_param(State#computer.mem, State#computer.pointer, State#computer.relative_base, 1),
  B = get_param(State#computer.mem, State#computer.pointer, State#computer.relative_base, 2),
  Value = if
            A == B -> 1;
            true -> 0
          end,
  NextMem = write(State#computer.mem, State#computer.pointer, State#computer.relative_base, 3, Value),
  NextState = State#computer{mem = NextMem, pointer = State#computer.pointer + 4},
  run(NextState).

base(State) ->
  A = get_param(State#computer.mem, State#computer.pointer, State#computer.relative_base, 1),
  NextState = State#computer{relative_base = State#computer.relative_base + A, pointer = State#computer.pointer + 2},
  run(NextState).

run(State) ->
  OpCode = read(State#computer.mem, State#computer.pointer) rem 100,
  master ! {op, OpCode, State#computer.pid, State#computer.mem, State#computer.pointer},
  case OpCode of
    1 ->
      add(State);
    2 ->
      mul(State);
    3 ->
      in(State);
    4 ->
      out(State);
    5 ->
      jmpnz(State);
    6 ->
      jmpz(State);
    7 ->
      lt(State);
    8 ->
      eq(State);
    9 ->
      base(State);
    99 ->
      State#computer.mem;
    _Else ->
      error({"Unrecognized OpCode", OpCode, State#computer.pointer})
  end.

read_memory(FileName) ->
  {ok, Bin} = file:read_file(FileName),
  Input = lists:map(fun(X) -> {Int, _} = string:to_integer(X),
    Int end,
    string:tokens(binary_to_list(Bin), ",")),
  WithIndex = lists:zip(lists:seq(0, length(Input) - 1), Input),
  maps:from_list(WithIndex).

spawn_computers(N, State) ->
  spawn_computers(N, State, []).

spawn_computers(0, _, Acc) -> Acc;
spawn_computers(N, State, Acc) ->
  NextState = State#computer{pid = N - 1},
  io:format("Spawned, "),
  NextVal = spawn(day_23, run, [NextState]),
%%  NextVal ! 9, % todo fix
%%  apply(day_23, run, [Mem, Pointer, RelBase, In, Out, State, N-1]),
%%  run(Mem, Pointer, RelBase, In, Out, State, N-1),
  spawn_computers(N - 1, State, [NextVal | Acc]).

count_messages(Addresses, Counter) ->
  receive
    {input, Sender} ->
      {ok, [X]} = io:fread("Input ", "~d"),
      Sender ! X;
    {output, Sender, N} ->
      io:format("Output from ~p => ~p ~n", [Sender, N]);
%%      receive
%%        {send, Sender, X} ->
%%          io:format("From: ~p X:~p ~n", [Sender, X]),
%%          receive
%%            {send, Sender, Y} ->
%%              Addr = lists:nth(N + 1, Addresses),
%%              io:format("From: ~p X:~p Y:~p ~n", [N, X, Y]),
%%              Addr ! X,
%%              Addr ! Y
%%         end
%%      end;
    {thx, Pid} ->
      io:format("Thx from ~p~n", [Pid]);
%%    {Sender, N, X, Y} ->
%%      io:format("From: ~p To: ~p  X:~p Y:~p ~n", [Sender, N, X, Y]),
%%      if
%%        N == 255 ->
%%          io:format("Answer X:~p Y:~p ~n", [X, Y]);
%%        true ->
%%          Addr = lists:nth((N rem 15) + 2, Addresses),
%%          Addr ! {X, Y}
%%      end
    {op, Op, Pid, Mem, Pointer} -> ok
%%        io:format("Op ~p on ~p ptr:~p ~p~n", [Op, Pid, Pointer, Mem])
  end,
  count_messages(Addresses, Counter + 1).

start_communication(Mem) ->
  State = #computer{mem = Mem},
  Addresses = spawn_computers(1, State),
%%  receive
%%    {Any} -> io:format("Any: ~p~n", [Any])
%%  end,
  count_messages(Addresses, 0).


%% escript Entry point

main(_) ->
  io:fwrite("Current version~n"),
  Mem = read_memory("day_23.in"),
  register(master, self()),
  start_communication(Mem),
%%  run(#computer{mem=Mem, next_val = 1}),
  io:format("finished").

%%====================================================================
%% Internal functions
%%====================================================================
