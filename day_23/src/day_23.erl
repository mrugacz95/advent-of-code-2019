-module(day_23).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

read(Mem, Addr) ->
  Val = maps:get(Addr, Mem, 0),
  Val.

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


run(Mem, Pointer, RelativeBase, Input, Output) ->
  OpCode = read(Mem, Pointer) rem 100,
  case OpCode of
    1 ->
      A = get_param(Mem, Pointer, RelativeBase, 1),
      B = get_param(Mem, Pointer, RelativeBase, 2),
      UpdatedMem = write(Mem, Pointer, RelativeBase, 3, A + B),
      run(UpdatedMem, Pointer + 4, RelativeBase, Input, Output);
    2 ->
      A = get_param(Mem, Pointer, RelativeBase, 1),
      B = get_param(Mem, Pointer, RelativeBase, 2),
      UpdatedMem = write(Mem, Pointer, RelativeBase, 3, A * B),
      run(UpdatedMem, Pointer + 4, RelativeBase, Input, Output);
    3 ->
      A = Input(),
      UpdatedMem = write(Mem, Pointer, RelativeBase, 1, A),
      run(UpdatedMem, Pointer + 2, RelativeBase, Input, Output);
    4 ->
      A = get_param(Mem, Pointer, RelativeBase, 1),
      Output(A),
      run(Mem, Pointer + 2, RelativeBase, Input, Output);
    5 ->
      A = get_param(Mem, Pointer, RelativeBase, 1),
      B = get_param(Mem, Pointer, RelativeBase, 2),
      if
        A /= 0 ->
          run(Mem, B, RelativeBase, Input, Output);
        true ->
          run(Mem, Pointer + 3, RelativeBase, Input, Output)
      end;
    6 ->
      A = get_param(Mem, Pointer, RelativeBase, 1),
      B = get_param(Mem, Pointer, RelativeBase, 2),
      if
        A == 0 ->
          run(Mem, B, RelativeBase, Input, Output);
        true ->
          run(Mem, Pointer + 3, RelativeBase, Input, Output)
      end;
    7 ->
      A = get_param(Mem, Pointer, RelativeBase, 1),
      B = get_param(Mem, Pointer, RelativeBase, 2),
      Value = if
                A < B -> 1;
                true -> 0
              end,
      UpdatedMem = write(Mem, Pointer, RelativeBase, 3, Value),
      run(UpdatedMem, Pointer + 4, RelativeBase, Input, Output);
    8 ->
      A = get_param(Mem, Pointer, RelativeBase, 1),
      B = get_param(Mem, Pointer, RelativeBase, 2),
      Value = if
                A == B -> 1;
                true -> 0
              end,
      UpdatedMem = write(Mem, Pointer, RelativeBase, 3, Value),
      run(UpdatedMem, Pointer + 4, RelativeBase, Input, Output);
    9 ->
      A = get_param(Mem, Pointer, RelativeBase, 1),
      run(Mem, Pointer + 2, RelativeBase + A, Input, Output);
    99 ->
      Mem;
    _Else ->
      error({"Unrecognized OpCode", OpCode, Pointer})
  end.


%% escript Entry point
main(_) ->
  In = fun() ->
    {ok, [X]} = io:fread("Input ", "~d"),
    X
       end,
  Out = fun(Value) ->
    io:format("Output: ~p~n", [Value])
        end,
  {ok, Bin} = file:read_file("day_23.in"),
  Input = lists:map(fun(X) -> {Int, _} = string:to_integer(X),
    Int end,
    string:tokens(binary_to_list(Bin), ",")),
  WithIndex = lists:zip(lists:seq(0, length(Input) - 1), Input),
  Mem = maps:from_list(WithIndex),
  Result = run(Mem, 0, 0, In, Out),
  erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
