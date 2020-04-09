-module(int_computer).

%% API
-export([run/2]).

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

loop(State) ->
  OpCode = read(State#computer.mem, State#computer.pointer) rem 100,
  case OpCode of
    1 ->
      A = get_param(State#computer.mem, State#computer.pointer, State#computer.relative_base, 1),
      B = get_param(State#computer.mem, State#computer.pointer, State#computer.relative_base, 2),
      NextMem = write(State#computer.mem, State#computer.pointer, State#computer.relative_base, 3, A + B),
      NextState = State#computer{mem = NextMem, pointer = State#computer.pointer + 4},
      loop(NextState);
    2 ->
      A = get_param(State#computer.mem, State#computer.pointer, State#computer.relative_base, 1),
      B = get_param(State#computer.mem, State#computer.pointer, State#computer.relative_base, 2),
      NextMem = write(State#computer.mem, State#computer.pointer, State#computer.relative_base, 3, A * B),
      NextState = State#computer{mem = NextMem, pointer = State#computer.pointer + 4},
      loop(NextState);
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
      loop(NextState);
    4 ->
      A = get_param(State#computer.mem, State#computer.pointer, State#computer.relative_base, 1),
      master ! {output, self(), A},
      NextState = State#computer{pointer = State#computer.pointer + 2},
      loop(NextState);
    5 ->
      A = get_param(State#computer.mem, State#computer.pointer, State#computer.relative_base, 1),
      B = get_param(State#computer.mem, State#computer.pointer, State#computer.relative_base, 2),
      Pointer = if
                  A /= 0 -> B;
                  true -> State#computer.pointer + 3
                end,
      NextState = State#computer{pointer = Pointer},
      loop(NextState);
    6 ->
      A = get_param(State#computer.mem, State#computer.pointer, State#computer.relative_base, 1),
      B = get_param(State#computer.mem, State#computer.pointer, State#computer.relative_base, 2),
      Pointer = if
                  A == 0 -> B;
                  true -> State#computer.pointer + 3
                end,
      NextState = State#computer{pointer = Pointer},
      loop(NextState);
    7 ->
      A = get_param(State#computer.mem, State#computer.pointer, State#computer.relative_base, 1),
      B = get_param(State#computer.mem, State#computer.pointer, State#computer.relative_base, 2),
      Value = if
                A < B -> 1;
                true -> 0
              end,
      NextMem = write(State#computer.mem, State#computer.pointer, State#computer.relative_base, 3, Value),
      NextState = State#computer{mem = NextMem, pointer = State#computer.pointer + 4},
      loop(NextState);
    8 ->
      A = get_param(State#computer.mem, State#computer.pointer, State#computer.relative_base, 1),
      B = get_param(State#computer.mem, State#computer.pointer, State#computer.relative_base, 2),
      Value = if
                A == B -> 1;
                true -> 0
              end,
      NextMem = write(State#computer.mem, State#computer.pointer, State#computer.relative_base, 3, Value),
      NextState = State#computer{mem = NextMem, pointer = State#computer.pointer + 4},
      loop(NextState);
    9 ->
      A = get_param(State#computer.mem, State#computer.pointer, State#computer.relative_base, 1),
      NextState = State#computer{relative_base = State#computer.relative_base + A, pointer = State#computer.pointer + 2},
      loop(NextState);
    99 ->
      State#computer.mem;
    _Else ->
      error({"Unrecognized OpCode", OpCode, State#computer.pointer})
  end.

run(Mem, Pid) ->
  loop(#computer{mem = Mem, pid = Pid}).