-module(day_23).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    io:format("Args: ~p~n", [Args]),
    {ok, Bin} = file:read_file("day_23.in"),
    Input = lists:map(fun(X) -> {Int, _} = string:to_integer(X),
                    Int end,
          string:tokens(binary_to_list(Bin), ",")),
    WithIndex = lists:zip(lists:seq(1, length(Input)), Input),
    Mem = maps:from_list(WithIndex),
    io:format("Args: ~p~n", [Mem]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
