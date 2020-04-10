-module(day_23_part_2).

%% API exports
-export([main/1, nat_proc/2, monitor_proc/2]).

%%====================================================================
%% API functions
%%====================================================================

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
spawn_computers(N, Mem, Acc) ->
%%  io:format("Spawned, "),
  NextVal = spawn(int_computer, run, [Mem, N - 1]),
  NextVal ! N - 1,
  spawn_computers(N - 1, Mem, [NextVal | Acc]).

count_messages(Addresses, Counter) ->
  Finished = receive
               {output, Sender, N} ->
%%                 io:format("Msg from ~p to ~p~n", [Sender, N]),
                 receive
                   {output, S, X} when S == Sender ->
%%                     io:format("Msg from ~p to ~p: X=~p~n", [Sender, N, X]),
                     receive
                       {output, S, Y} when S == Sender ->
                         if
                           N == 256 ->
                             io:format("Gracefully finishing~n"),
                             [exit(E, ok) || E <- Addresses],
                             exit(whereis(nat), ok),
                             io:format("Answer is ~p~n", [Y]),
                             true;
                           N == 255 ->
%%                             io:format("Nat hit with ~p ~p~n", [X, Y]),
                             nat ! {post, X, Y},
                             false;
                           true ->
                             Addr = lists:nth(N + 1, Addresses),
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

monitor_proc(Addresses, Y) ->
  Is_Idle = fun(Pid) ->
    {message_queue_len, Len} = process_info(Pid, message_queue_len),
    Len == 0
            end,
  Idle = lists:all(Is_Idle, Addresses),
  {SentY, Finished} = if Idle ->
    nat ! {get, self()},
    receive
      {RecvX, RecvY} ->
        if
          RecvY /= Y -> % resume network
            master ! {output, self(), 0}, % receiver
            master ! {output, self(), RecvX}, % X
            master ! {output, self(), RecvY}, % Y
            {RecvY, false};
          true -> % finish computation
            master ! {output, self(), 256}, % receiver
            master ! {output, self(), Y}, % X
            master ! {output, self(), Y}, % Y
            {RecvY, true}
        end
    end;
                        true -> {Y, false}
                      end,
  if
    Finished ->
      ok;
    true ->
      timer:sleep(50), % wait till next idle check
      monitor_proc(Addresses, SentY)
  end.

nat_proc(X, Y) ->
  receive
    {post, RecvX, RecvY} ->
      nat_proc(RecvX, RecvY);
    {get, Sender} ->
      Sender ! {X, Y},
      nat_proc(X, Y)
  end.
%% escript Entry point

main(_) ->
  io:fwrite("Part 2~n"),
  Mem = read_memory("day_23.in"),
  register(master, self()),
  Addresses = spawn_computers(50, Mem),
  Nat_Pid = spawn(day_23_part_2, nat_proc, [null, null]),
  spawn(day_23_part_2, monitor_proc, [Addresses, null]),
  register(nat, Nat_Pid),
  count_messages(Addresses, 0).

%%====================================================================
%% Internal functions
%%====================================================================
