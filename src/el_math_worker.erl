-module(el_math_worker).

-export([start_link/0]).

start_link() ->
    case el_math:start() of
        {_, _, wxFrame, Pid} ->
            {ok, Pid};
        Other ->
            io:format("Other Start: ~p~n", [Other]),
            error
    end.
