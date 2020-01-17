-module(el_math_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    el_math_sup:start_link().

stop(_State) ->
	ok.
