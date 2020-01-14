%%% @author Taras J. Honcharuk
%%% @copyright (C) 2020, TheLostGameTeam
%%%  
%%% LICENSE GPLv3
%%%
%%%

-module(el_math).

-behaviour(wx_object).

-export([start/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2, terminate/2, code_change/3]).

-export([mul/2, mydiv/2, add/2, sub/2]).

-include_lib("wx/include/wx.hrl").

-define(wxID_TASK, 2).
-define(wxID_ANSWER, 3).
-define(wxID_BUTTON, 900).
-define(wxID_RESULT, 4).
-define(wxID_RANDOM, 5).


-define(SERVER, ?MODULE).

-record(state, {command = 302,
                action = mul,
                a = 2,
                b = 2,
                right = 0,
                wrong = 0,
                frame,
                timer_set = disable,
                timer = disable,
                rand_list = [],
                random = true
               }).

start() ->
%    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
    wx_object:start_link({local, ?SERVER}, ?MODULE, [], []).

init(_) ->
    Wx = wx:new(),
    Frame = wx:batch(fun() -> create_frame(Wx) end),
    wxWindow:show(Frame),

%    {ok, #state{}}.


    {Frame, #state{frame = Frame}}.

create_frame(Wx) ->

    Frame = wxFrame:new(Wx, 1, "Арифметика"),
    wxFrame:connect(Frame, close_window),


    Menu = create_menu(),
    TimerMenu = create_timer_menu(),

    MenuBar    = wxMenuBar:new(?wxMB_DOCKABLE),
    wxMenuBar:append(MenuBar, Menu, "Оберіть дію"),
    wxMenuBar:append(MenuBar, TimerMenu, "Час"),
    wxFrame:setMenuBar(Frame, MenuBar),

    ok = wxFrame:connect(Frame, command_menu_selected),

    wxFrame:createStatusBar(Frame,[]),
    Status = io_lib:format("Вірно: ~p      Невірно: ~p         Залишилось часу: ~p", [0,0,disable]),
    ok = wxFrame:setStatusText(Frame, Status,[]),

    Label = wxStaticText:new(Frame, ?wxID_TASK, "", [{pos, {0,0}}, {size,{250, 60}}]),
    Counter = wxTextCtrl:new(Frame, ?wxID_ANSWER, [{value, ""}, {pos, {250, 5}}, {size, {150, 60}},  {style, ?wxTE_RIGHT}]),
    
    Font = wxFont:new(42, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_BOLD),

    Button = wxButton:new(Frame, ?wxID_BUTTON, [{label, "Перевірити"}, {pos, {400, 5}}, {size, {150, 50}}]),
    
    Result = wxStaticText:new(Frame, ?wxID_RESULT, "", [{pos, {550,0}}, {size,{400, 60}}]),
    wxStaticText:setFont(Label, Font),
    wxStaticText:setFont(Result, Font),
    wxTextCtrl:setFont(Counter, Font),

    MainSizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(MainSizer, Label, [{flag, ?wxALL bor ?wxALIGN_CENTRE},{border, 1}]),
    wxSizer:add(MainSizer, Counter, [{flag, ?wxALL bor ?wxALIGN_CENTRE},{border, 1}]),
    wxSizer:add(MainSizer, Button, [{flag, ?wxALL bor ?wxALIGN_CENTRE},{border, 1}]),
    wxSizer:add(MainSizer, Result, [{flag, ?wxALL bor ?wxALIGN_CENTRE},{border, 1}]),
    wxWindow:setSizer(Frame, MainSizer),
    wxSizer:setSizeHints(MainSizer, Frame),


    wxFrame:connect(Frame, command_button_clicked),
    Frame.

start(Frame, Command, A) ->
    Action = get_action(integer_to_list(Command)),
    timer_start(0),
    Button = get_object(?wxID_BUTTON, Frame),
    wxButton:enable(Button),
    start(Frame, Command, Action, A).

start(Frame, Command, Action0, R) ->

    Action =
    case Command of
        500 -> get_action(integer_to_list(Command));
        _ -> Action0
    end,

%    io:format("start command: ~p, action: ~p~n", [Command, Action]),

    Task = get_object(?wxID_TASK, Frame),
    Answer = get_object(?wxID_ANSWER, Frame),
    wxWindow:setFocus(Answer),
    

    {A, B} = get_params(Action, Command, R),

    wxStaticText:setLabel(Task, [integer_to_list(A), sign(Action), integer_to_list(B), " = "]),
    wxTextCtrl:setValue(Answer, ""),

    {A, B, Action}.

update_status(#state{frame = Frame, right = R, wrong = W, timer = Time}) ->
    Status = io_lib:format("Вірно: ~p      Невірно: ~p         Залишилось часу: ~p", [R,W, Time]),
    wxFrame:setStatusText(Frame, Status, []).

update_status(Frame, #state{right = R, wrong = W, timer = Time}) ->
    Status = io_lib:format("Вірно: ~p      Невірно: ~p         Залишилось часу: ~p", [R,W, Time]),
    wxFrame:setStatusText(Frame, Status, []).

disable_button(#state{frame = Frame}) ->
    Button = get_object(?wxID_BUTTON, Frame),
    wxButton:disable(Button).

check(Frame, #state{a = A, b = B, action = Action, command = Command, right = R, wrong = W, rand_list = Random} = State) ->
    Answer = get_object(?wxID_ANSWER, Frame),
    TextRes = wxTextCtrl:getValue(Answer),
    Res =
    try 
        list_to_integer(TextRes)
    catch _:Err ->
              io:format("Err: ~p~n", [Err]),
              0
    end,
%    io:format("Random: ~p~n", [Random]),
%    io:format("~p ~p ~p = ~p~n", [A, sign(Action), B, Res]),
    Result = get_object(?wxID_RESULT, Frame),
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
    


handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.
handle_info({timeout, _, timer}, #state{timer_set = disable} = State) ->
    {noreply, State};
handle_info({timeout, _, timer}, #state{timer = 0} = State) ->
    disable_button(State),
    {noreply, State#state{timer = State#state.timer_set}};
handle_info({timeout, _, timer}, #state{timer = Time} = State) ->
    erlang:start_timer(1000, self(), timer),
    State1 = State#state{timer = Time - 1},
    update_status(State1),
    {noreply, State1};
handle_info(Msg, State) ->
    io:format("Got info: ~p~n", [Msg]),
    {noreply, State}.

handle_event(#wx{event=#wxClose{}, obj=Frame}, State) ->
    io:format("~p Closing window ~n",[self()]),
    wxWindow:destroy(Frame),
    {stop, normal, State};
handle_event(#wx{id = Id}, State) when Id div 100 =:= 6 ->
    Time = timer(Id),
    {noreply, State#state{timer =  Time, timer_set = Time}};
handle_event(#wx{obj=Menu, id=?wxID_RANDOM}, State) ->
    case wxMenu:isChecked(Menu, ?wxID_RANDOM) of
        true ->
            {noreply, State#state{random = true}};
        _ ->
            {noreply, State#state{random = false}}
    end;
handle_event(#wx{obj=Frame, id=Id,  userData=UserData, event=#wxCommand{type=command_menu_selected}}, State)->
    [R|Tail] = get_random_and_tail(State#state.random),
%    io:format("got wx:~p ud:~p~n", [Wx, UserData]),
    {A, B, Action} = start(Frame, Id, R),
    State1 = State#state{a = A,
                         b = B,
                         action = Action,
                         command = Id,
                         right = 0,
                         wrong =0,
                         rand_list = Tail
                        },
    {noreply, State1};
handle_event(#wx{obj=Frame, id=?wxID_BUTTON}, State) ->
    State1 = check(Frame, State),
    update_status(Frame, State1),
    {noreply, State1};
handle_event(Msg, State) ->
    io:format("Got event ~p ~n", [Msg]),
    {noreply, State}.

terminate(_, _) ->
    io:format("terminate~n"),
    wx:destroy(),
    ok.

code_change(_, _, State) ->
    {ok, State}.


get_random_and_tail(true) ->
    [0|[]];
get_random_and_tail(false) ->
    shuffle(lists:seq(1,9)).

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
%    A = random(),
    {B + 1, rand:uniform(B)};
get_params(sub, 202, M) ->
%    M = random(),
    B = rand:uniform(10 - M) + M - 1,
    {M + 10, B};
get_params(sub, _, A) ->
%    A = random(),
    {random() + A, A};
get_params(add, 101, A) ->
%    A = random(),
    B = rand:uniform(10 - A),
    {A, B};
get_params(add, 102, A) ->
%    A = rand:uniform(4) + rand:uniform(5),
    B = rand:uniform(A) + 10 - A,
    {A, B};
get_params(_, _, A) ->
    {A, random()}.



random() ->
    rand:uniform(5) + rand:uniform(5) - 1.

random_tuple() ->
    {random(), random()}.

shuffle(List) ->
    shuffle(List, []).

shuffle([], Acc) ->
    Acc;
shuffle(List, Acc) ->
    N = lists:nth(rand:uniform(length(List)), List),
    shuffle(lists:delete(N, List), [N|Acc]).

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

timer(600) ->
    disable;
timer(630) ->
    30;
timer(Command) ->
    Min = Command rem 10,
    Time = Min * 60,
    Time.

timer_start(#state{timer = 0}) ->
    ok;
timer_start(_) ->
    erlang:start_timer(1000, self(), timer).

get_object(Id, Frame) ->
    Object = wxWindow:findWindowById(Id, [{parent, Frame}]),
    case Id of
        ?wxID_ANSWER ->
            wx:typeCast(Object, wxTextCtrl);
        ?wxID_BUTTON ->
            wx:typeCast(Object, wxButton);
        _ ->
            wx:typeCast(Object, wxStaticText)
    end.

create_menu() ->
    Menu = wxMenu:new([]),
    SubMenuAdd = wxMenu:new([]),
    SubMenuSub = wxMenu:new([]),
    SubMenuMul  = wxMenu:new([]),
    SubMenuDiv = wxMenu:new([]),

    wxMenu:appendCheckItem(Menu, ?wxID_RANDOM, "Випадково", []),
    wxMenu:break(Menu),
    wxMenu:append(SubMenuAdd, 101, "До 10", []),
    wxMenu:append(SubMenuAdd, 102, "З переходом через 10", []),
    wxMenu:append(SubMenuAdd, 100, "Усі", []),

    wxMenu:append(Menu, ?wxID_ANY, "Додавання", SubMenuAdd, []),


    wxMenu:append(SubMenuSub, 201, "До 10", []),
    wxMenu:append(SubMenuSub, 202, "З переходом через 10", []),
    wxMenu:append(SubMenuSub, 200, "Усі", []),

    wxMenu:append(Menu, ?wxID_ANY, "Віднімання", SubMenuSub, []),

    
    wxMenu:append(SubMenuMul, 301, "x1", []),
    wxMenu:append(SubMenuMul, 302, "x2", []),
    wxMenu:append(SubMenuMul, 303, "x3", []),
    wxMenu:append(SubMenuMul, 304, "x4", []),
    wxMenu:append(SubMenuMul, 305, "x5", []),
    wxMenu:append(SubMenuMul, 306, "x6", []),
    wxMenu:append(SubMenuMul, 307, "x7", []),
    wxMenu:append(SubMenuMul, 308, "x8", []),
    wxMenu:append(SubMenuMul, 309, "x9", []),
    wxMenu:break(SubMenuMul),
    wxMenu:append(SubMenuMul, 300, "Усі", []),
    
    wxMenu:append(Menu, ?wxID_ANY, "Множення", SubMenuMul, []),

    
    wxMenu:append(SubMenuDiv, 401, ":1", []),
    wxMenu:append(SubMenuDiv, 402, ":2", []),
    wxMenu:append(SubMenuDiv, 403, ":3", []),
    wxMenu:append(SubMenuDiv, 404, ":4", []),
    wxMenu:append(SubMenuDiv, 405, ":5", []),
    wxMenu:append(SubMenuDiv, 406, ":6", []),
    wxMenu:append(SubMenuDiv, 407, ":7", []),
    wxMenu:append(SubMenuDiv, 408, ":8", []),
    wxMenu:append(SubMenuDiv, 409, ":9", []),
    wxMenu:break(SubMenuDiv),
    wxMenu:append(SubMenuDiv, 400, "Усі", []),

    wxMenu:append(Menu, ?wxID_ANY, "Ділення", SubMenuDiv, []),
    wxMenu:break(Menu),
    wxMenu:append(Menu, 500, "Усі", []),

    wxMenu:check(Menu, ?wxID_RANDOM, true),
    wxMenu:connect(Menu, command_menu_selected),


    Menu.

create_timer_menu() ->
    Menu = wxMenu:new(),

    wxMenu:appendRadioItem(Menu, 600, "Без обмеження", []),
    
    wxMenu:break(Menu),

    wxMenu:appendRadioItem(Menu, 630, "30 сек", []),
    wxMenu:appendRadioItem(Menu, 601, "1 хв", []),
    wxMenu:appendRadioItem(Menu, 602, "2 хв", []),
    wxMenu:appendRadioItem(Menu, 603, "3 хв", []),
    wxMenu:appendRadioItem(Menu, 605, "5 хв", []),

    Menu.

%create_start_menu() ->
%    Menu = wxMenu:new(),
%
%    wxFrame:connect(Menu, command_menu_selected),
%    Menu.
