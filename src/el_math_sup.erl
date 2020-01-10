-module(el_math_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Math = {el_math_worker, {el_math_worker, start_link, []},
           permanent, 5000, worker, [el_math_worker]},
	Procs = [Math],
	{ok, {{one_for_one, 1, 5}, Procs}}.
