-module(day_23_part_2).

%% API exports
-export([main/1]).

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
                           N == 255 -> io:format("Answer is ~p~n", [Y]),
                             [exit(E, ok) || E <- Addresses],
                             true;
                           true ->
                             Addr = lists:nth(N + 1, Addresses),
%%                             io:format("Msg from ~p to ~p: X=~p,Y=~p~n", [Sender, N, X, Y]),
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


%% escript Entry point

main(_) ->
  io:fwrite("Part 2~n"),
  Mem = read_memory("day_23.in"),
  register(master, self()),
  Addresses = spawn_computers(50, Mem),
  count_messages(Addresses, 0).

%%====================================================================
%% Internal functions
%%====================================================================
