-module(task).

-export([start/3]).
-export([check/1]).
-export([add/2, sub/2, mul/2, mydiv/2]).

-include_lib("wx/include/wx.hrl").
-include("el_math.hrl").

start(Frame, Command, A) ->
    Action = get_action(integer_to_list(Command)),
    timer_start(0),
    Button = widgets:get_object(?wxID_BUTTON, Frame),
    wxButton:enable(Button),
    start(Frame, Command, Action, A).

start(Frame, Command, Action0, R) ->

    Action =
    case Command of
        500 -> get_action(integer_to_list(Command));
        _ -> Action0
    end,


    Task = widgets:get_object(?wxID_TASK, Frame),
    Answer = widgets:get_object(?wxID_ANSWER, Frame),
    wxWindow:setFocus(Answer),
    

    {A, B} = get_params(Action, Command, R),

    wxStaticText:setLabel(Task, [integer_to_list(A), sign(Action), integer_to_list(B), " = "]),
    wxTextCtrl:setValue(Answer, ""),

    {A, B, Action}.

check(#state{frame = Frame,
             a = A, b = B,
             action = Action,
             command = Command,
             right = R, wrong = W,
             rand_list = Random} = State) ->
    Answer = widgets:get_object(?wxID_ANSWER, Frame),
    TextRes = wxTextCtrl:getValue(Answer),
    Res =
    try 
        list_to_integer(TextRes)
    catch _:Err ->
              io:format("Err: ~p~n", [Err]),
              0
    end,
    Result = widgets:get_object(?wxID_RESULT, Frame),
    case apply(?MODULE, Action, [A, B]) of
        Res when Random =/= [] ->
            wxStaticText:setLabel(Result, "Вірно"),
            [H|T] = Random,
            {A1, B1, Action1} = start(Frame, Command, Action, H),
            State#state{a = A1, b = B1, action = Action1, right = R + 1, rand_list = T};
        Res ->
            wxStaticText:setLabel(Result, "Вірно"),
            {A1, B1, Action1} = start(Frame, Command, Action, 0),
            State#state{a = A1, b = B1, action = Action1, right = R + 1};
        _ ->
            wxStaticText:setLabel(Result, "Невірно"),
            wxTextCtrl:setValue(Answer, ""),
            wxWindow:setFocus(Answer),
            State#state{wrong = W + 1}
    end.




%%%%%%%%%%%%%%%%%%% INTERNAL %%%%%%%%%%%%%%%%%%%%%%


mul(A, B) ->
    A * B.
add(A, B) ->
    A + B.
sub(A, B) ->
    A - B.
mydiv(A, B) ->
    A div B.

get_params(mydiv, Command, 0) when Command rem 10 =:= 0 ->
    A = random(),
    {random() * A, A};
get_params(mydiv, Command, 0) ->
    A = Command rem 10,
    {random() * A, A};
get_params(mul, Command, 0) when Command rem 10 =:= 0 ->
    random_tuple();
get_params(mul, Commamd, 0) ->
    A = Commamd rem 10,
    {A, random()};
get_params(sub, 201, 0) ->
    A = random(),
    {A + 1, rand:uniform(A)};
get_params(sub, 202, 0) ->
    M = random(),
    B = rand:uniform(10 - M) + M - 1,
    {M + 10, B};
get_params(sub, _, 0) ->
    A = random(),
    {random() + A, A};
get_params(add, 101, 0) ->
    A = random(),
    B = rand:uniform(10 - A),
    {A, B};
get_params(add, 102, 0) ->
    A = rand:uniform(4) + rand:uniform(5),
    B = rand:uniform(A - 1) + 10 - A,
    {A, B};
get_params(_, _, 0) ->
    random_tuple();

get_params(mydiv, Command, B) when Command rem 10 =:= 0 ->
    {random() * B, B};
get_params(mydiv, Command, A) ->
    B = Command rem 10,
    {A * B, B};
get_params(mul, Command, B) when Command rem 10 =:= 0 ->
    {B, random()};
get_params(mul, Commamd, B) ->
    A = Commamd rem 10,
    {A, B};
get_params(sub, 201, B) ->
    {B + 1, rand:uniform(B)};
get_params(sub, 202, M) ->
    B = rand:uniform(10 - M) + M - 1,
    {M + 10, B};
get_params(sub, _, A) ->
    {random() + A, A};
get_params(add, 101, A) ->
    B = rand:uniform(10 - A),
    {A, B};
get_params(add, 102, A) ->
    B = rand:uniform(A) + 10 - A,
    {A, B};
get_params(_, _, A) ->
    {A, random()}.



random() ->
    rand:uniform(5) + rand:uniform(5) - 1.

random_tuple() ->
    {random(), random()}.

sign(mul) ->
    " x ";
sign(mydiv) ->
    " : ";
sign(add) ->
    " + ";
sign(sub) ->
    " - ".

get_action([$1 |_]) ->
    add;
get_action([$2 |_]) ->
    sub;
get_action([$3 |_]) ->
    mul;
get_action([$4 |_]) ->
    mydiv;
get_action([$5 |_]) ->
    lists:nth(rand:uniform(4), [mul, mydiv, add, sub]);
get_action(_) ->
    get_action([$5]).


%timer_start(#state{timer = 0}) ->
%    ok;
timer_start(_) ->
    erlang:start_timer(1000, self(), timer).

