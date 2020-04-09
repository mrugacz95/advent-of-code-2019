-module(day_23).

%% API exports
-export([main/1, run/1]).

%%====================================================================
%% API functions
%%====================================================================
-record(computer, {mem, pointer = 0, relative_base = 0, pid = -1, queue = []}).

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

run(State) ->
  OpCode = read(State#computer.mem, State#computer.pointer) rem 100,
  case OpCode of
    1 ->
      A = get_param(State#computer.mem, State#computer.pointer, State#computer.relative_base, 1),
      B = get_param(State#computer.mem, State#computer.pointer, State#computer.relative_base, 2),
      NextMem = write(State#computer.mem, State#computer.pointer, State#computer.relative_base, 3, A + B),
      NextState = State#computer{mem = NextMem, pointer = State#computer.pointer + 4},
      run(NextState);
    2 ->
      A = get_param(State#computer.mem, State#computer.pointer, State#computer.relative_base, 1),
      B = get_param(State#computer.mem, State#computer.pointer, State#computer.relative_base, 2),
      NextMem = write(State#computer.mem, State#computer.pointer, State#computer.relative_base, 3, A * B),
      NextState = State#computer{mem = NextMem, pointer = State#computer.pointer + 4},
      run(NextState);
    3 ->
      {message_queue_len, Len} = process_info(self(), message_queue_len),
      NextState = if
                    Len == 0 ->
                      NextMem = write(State#computer.mem, State#computer.pointer, State#computer.relative_base, 1, -1),
                      State#computer{mem = NextMem, pointer = State#computer.pointer + 2};
                    true ->
                      receive
                        X ->
                          NextMem = write(State#computer.mem, State#computer.pointer, State#computer.relative_base, 1, X),
                          State#computer{mem = NextMem, pointer = State#computer.pointer + 2}
                      end
                  end,
      run(NextState);
    4 ->
      A = get_param(State#computer.mem, State#computer.pointer, State#computer.relative_base, 1),
      master ! {output, self(), A},
      NextState = State#computer{pointer = State#computer.pointer + 2},
      run(NextState);
    5 ->
      A = get_param(State#computer.mem, State#computer.pointer, State#computer.relative_base, 1),
      B = get_param(State#computer.mem, State#computer.pointer, State#computer.relative_base, 2),
      Pointer = if
                  A /= 0 -> B;
                  true -> State#computer.pointer + 3
                end,
      NextState = State#computer{pointer = Pointer},
      run(NextState);
    6 ->
      A = get_param(State#computer.mem, State#computer.pointer, State#computer.relative_base, 1),
      B = get_param(State#computer.mem, State#computer.pointer, State#computer.relative_base, 2),
      Pointer = if
                  A == 0 -> B;
                  true -> State#computer.pointer + 3
                end,
      NextState = State#computer{pointer = Pointer},
      run(NextState);
    7 ->
      A = get_param(State#computer.mem, State#computer.pointer, State#computer.relative_base, 1),
      B = get_param(State#computer.mem, State#computer.pointer, State#computer.relative_base, 2),
      Value = if
                A < B -> 1;
                true -> 0
              end,
      NextMem = write(State#computer.mem, State#computer.pointer, State#computer.relative_base, 3, Value),
      NextState = State#computer{mem = NextMem, pointer = State#computer.pointer + 4},
      run(NextState);
    8 ->
      A = get_param(State#computer.mem, State#computer.pointer, State#computer.relative_base, 1),
      B = get_param(State#computer.mem, State#computer.pointer, State#computer.relative_base, 2),
      Value = if
                A == B -> 1;
                true -> 0
              end,
      NextMem = write(State#computer.mem, State#computer.pointer, State#computer.relative_base, 3, Value),
      NextState = State#computer{mem = NextMem, pointer = State#computer.pointer + 4},
      run(NextState);
    9 ->
      A = get_param(State#computer.mem, State#computer.pointer, State#computer.relative_base, 1),
      NextState = State#computer{relative_base = State#computer.relative_base + A, pointer = State#computer.pointer + 2},
      run(NextState);
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
  NextVal ! N - 1,
  spawn_computers(N - 1, State, [NextVal | Acc]).

count_messages(Addresses, Counter) ->
  Finished = receive
               {output, Sender, N} ->
                 io:format("Msg from ~p to ~p~n", [Sender, N]),
                 receive
                   {output, S, X} when S == Sender ->
                     io:format("Msg from ~p to ~p: X=~p~n", [Sender, N, X]),
                     receive
                       {output, S, Y} when S == Sender ->
                         if
                           N == 255 -> io:format("Answer is ~p~n", [Y]),
                             [exit(E, ok) || E <- Addresses],
                             true;
                           true ->
                             Addr = lists:nth(N + 1, Addresses),
                             io:format("Msg from ~p to ~p: X=~p,Y=~p~n", [Sender, N, X, Y]),
                             Addr ! X,
                             Addr ! Y,
                             false
                         end
                     end
                 end
             end,
  if Finished == true -> ok;
    true -> count_messages(Addresses, Counter + 1)
  end.

start_communication(Mem) ->
  State = #computer{mem = Mem},
  Addresses = spawn_computers(50, State),
  io:format("Adresses ~p~n", [Addresses]),
  count_messages(Addresses, 0).


%% escript Entry point

main(_) ->
  io:fwrite("Current version~n"),
  Mem = read_memory("day_23.in"),
  register(master, self()),
  start_communication(Mem).

%%====================================================================
%% Internal functions
%%====================================================================
