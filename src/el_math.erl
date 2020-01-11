-module(el_math).

-behaviour(wx_object).

-export([start/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_event/2, terminate/2, code_change/3]).

-export([mul/2, mydiv/2, add/2, sub/2]).

-include_lib("wx/include/wx.hrl").

-define(wxID_TASK, 2).
-define(wxID_ANSWER, 3).
-define(wxID_BUTTON, 900).
-define(wxID_RESULT, 5).

-define(SERVER, ?MODULE).

-record(state, {command = 302,
                action = mul,
                a = 2,
                b = 2,
                right = 0,
                wrong = 0,
                frame,
                timer_set = disable,
                timer = 0
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
%    StartButton = create_start_menu(),

    MenuBar    = wxMenuBar:new(?wxMB_DOCKABLE),
    wxMenuBar:append(MenuBar, Menu, "Оберіть дію"),
    wxMenuBar:append(MenuBar, TimerMenu, "Час"),
%    wxMenuBar:append(MenuBar, StartButton, "Start"),
    wxFrame:setMenuBar(Frame, MenuBar),

    ok = wxFrame:connect(Frame, command_menu_selected),

    wxFrame:createStatusBar(Frame,[]),
    Status = io_lib:format("Вірно: ~p      Невірно: ~p         Залишилось часу: ~p", [0,0,0]),
%    Status = io_lib:format("Вірно: ~p      Невірно: ~p", [0,0]),
    ok = wxFrame:setStatusText(Frame, Status,[]),

    Label = wxStaticText:new(Frame, ?wxID_TASK, "", [{pos, {0,0}}, {size,{250, 50}}]),
    Counter = wxTextCtrl:new(Frame, ?wxID_ANSWER, [{value, ""}, {pos, {250, 5}}, {size, {150, 50}},  {style, ?wxTE_RIGHT}]),
    
    Font = wxFont:new(42, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_BOLD),

    Button = wxButton:new(Frame, ?wxID_BUTTON, [{label, "Перевірити"}, {pos, {400, 5}}, {size, {150, 50}}]),
    
    Result = wxStaticText:new(Frame, ?wxID_RESULT, "", [{pos, {550,0}}, {size,{400, 50}}]),
    wxStaticText:setFont(Label, Font),
    wxStaticText:setFont(Result, Font),
    wxTextCtrl:setFont(Counter, Font),

    MainSizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(MainSizer, Label, [{border, 1}]),
    wxSizer:add(MainSizer, Counter, [{border, 1}]),
    wxSizer:add(MainSizer, Button, [{border, 1}]),
    wxSizer:add(MainSizer, Result, [{border, 1}]),
%    wxSizer:add(MainSizer, Label, [{flag, ?wxALL bor ?wxALIGN_CENTRE},{border, 1}]),
%    wxSizer:add(MainSizer, Counter, [{flag, ?wxALL bor ?wxALIGN_CENTRE},{border, 1}]),
%    wxSizer:add(MainSizer, Button, [{flag, ?wxALL bor ?wxALIGN_CENTRE},{border, 1}]),
%    wxSizer:add(MainSizer, Result, [{flag, ?wxALL bor ?wxALIGN_CENTRE},{border, 1}]),
    wxWindow:setSizer(Frame, MainSizer),
    wxSizer:setSizeHints(MainSizer, Frame),


    wxFrame:connect(Frame, command_button_clicked),
    Frame.

start(Frame, Command) ->
    Action = get_action(integer_to_list(Command)),
    timer_start(0),
    Button = get_object(?wxID_BUTTON, Frame),
    io:format("Button: ~p~n", [Button]),
    wxButton:enable(Button),
    start(Frame, Command, Action).

start(Frame, Command, Action0) ->

    Action =
    case Command of
        500 -> get_action(integer_to_list(Command));
        _ -> Action0
    end,

    io:format("start command: ~p, action: ~p~n", [Command, Action]),

    Task = get_object(?wxID_TASK, Frame),
    Answer = get_object(?wxID_ANSWER, Frame),
    wxWindow:setFocus(Answer),
    

    {A, B} = get_params(Action, Command),

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

check(Frame, #state{a = A, b = B, action = Action, command = Command, right = R, wrong = W} = State) ->
    Answer = get_object(?wxID_ANSWER, Frame),
    TextRes = wxTextCtrl:getValue(Answer),
    Res =
    try 
        list_to_integer(TextRes)
    catch _:Err ->
              io:format("Err: ~p~n", [Err]),
              0
    end,

    io:format("~p ~p ~p = ~p~n", [A, sign(Action), B, Res]),
    Result = get_object(?wxID_RESULT, Frame),
    case apply(?MODULE, Action, [A, B]) of
        Res ->
            wxStaticText:setLabel(Result, "Вірно"),
            {A1, B1, Action1} = start(Frame, Command, Action),
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
handle_event(#wx{obj=Frame, id=Id,  userData=UserData, event=#wxCommand{type=command_menu_selected}} = Wx, State)->
    io:format("got wx:~p ud:~p~n", [Wx, UserData]),
    {A, B, Action} = start(Frame, Id),
    State1 = State#state{a = A,
                         b = B,
                         action = Action,
                         command = Id,
                         right = 0,
                         wrong =0},
    {noreply, State1};
handle_event(#wx{obj=Frame, id=?wxID_BUTTON}, State) ->
    State1 = check(Frame, State),
    update_status(Frame, State1),
%    {A, B, _} = start(Frame, State#state.command, State#state.action),
    {noreply, State1};
handle_event(Msg, State) ->
    io:format("Got event ~p ~n", [Msg]),
    {noreply, State}.

terminate(_, _) ->
    wx:destroy(),
    ok.

code_change(_, _, State) ->
    {ok, State}.


mul(A, B) ->
    A * B.
add(A, B) ->
    A + B.
sub(A, B) ->
    A - B.
mydiv(A, B) ->
    A div B.

get_params(mydiv, Command) when Command rem 10 =:= 0 ->
    A = rand:uniform(9),
    {rand:uniform(9) * A, A};
get_params(mydiv, Command) ->
    A = Command rem 10,
    {rand:uniform(9) * A, A};
get_params(mul, Command) when Command rem 10 =:= 0 ->
    random();
get_params(mul, Commamd) ->
    A = Commamd rem 10,
    {A, rand:uniform(9)};
get_params(sub, _) ->
    A = rand:uniform(9),
    {rand:uniform(9) + A, A};
get_params(_, _) ->
    random().

random() ->
    {rand:uniform(9), rand:uniform(9)}.

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
    SubMenu  = wxMenu:new([]),
    SubMenu2 = wxMenu:new([]),

    wxMenu:append(Menu, 100, "Додавання", []),
    
    wxMenu:append(Menu, 200, "Віднімання", []),

    wxMenu:append(SubMenu, 301, "x1", []),
    wxMenu:append(SubMenu, 302, "x2", []),
    wxMenu:append(SubMenu, 303, "x3", []),
    wxMenu:append(SubMenu, 304, "x4", []),
    wxMenu:append(SubMenu, 305, "x5", []),
    wxMenu:append(SubMenu, 306, "x6", []),
    wxMenu:append(SubMenu, 307, "x7", []),
    wxMenu:append(SubMenu, 308, "x8", []),
    wxMenu:append(SubMenu, 309, "x9", []),
    wxMenu:break(SubMenu),
    wxMenu:append(SubMenu, 300, "Усі", []),
    
    wxMenu:append(Menu, ?wxID_ANY, "Множення", SubMenu, []),

    wxMenu:append(SubMenu2, 401, ":1", []),
    wxMenu:append(SubMenu2, 402, ":2", []),
    wxMenu:append(SubMenu2, 403, ":3", []),
    wxMenu:append(SubMenu2, 404, ":4", []),
    wxMenu:append(SubMenu2, 405, ":5", []),
    wxMenu:append(SubMenu2, 406, ":6", []),
    wxMenu:append(SubMenu2, 407, ":7", []),
    wxMenu:append(SubMenu2, 408, ":8", []),
    wxMenu:append(SubMenu2, 409, ":9", []),
    wxMenu:break(SubMenu),
    wxMenu:append(SubMenu2, 400, "Усі", []),

    wxMenu:append(Menu, ?wxID_ANY, "Ділення", SubMenu2, []),
    wxMenu:break(Menu),
    wxMenu:append(Menu, 500, "Усі", []),


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

create_start_menu() ->
    Menu = wxMenu:new(),

    wxFrame:connect(Menu, command_menu_selected),
    Menu.
