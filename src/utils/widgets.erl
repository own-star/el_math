%%% @author Taras J. Honcharuk
%%% @copyright (C) 2020, TheLostGameTeam
%%%  
%%% LICENSE GPLv3
%%%
%%%

-module(widgets).

-export([create_frame/1]).
%-export([create_menu/0, create_timer_menu/0]).
-export([get_object/2, update_status/1, disable_button/1]).

-include_lib("wx/include/wx.hrl").
-include("el_math.hrl").

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
    Status = io_lib:format("Вірно: ~p      Невірно: ~p         Залишилось часу: ~p",
                           [0,0,disable]),
    ok = wxFrame:setStatusText(Frame, Status,[]),

    Label = wxStaticText:new(Frame, ?wxID_TASK, "", [{pos, {0,0}},
                                                     {size,{250, 60}}]),
    Counter = wxTextCtrl:new(Frame, ?wxID_ANSWER, [{value, ""},
                                                   {pos, {250, 5}},
                                                   {size, {150, 60}},
                                                   {style, ?wxTE_RIGHT}]),
    
    Font = wxFont:new(42, ?wxFONTFAMILY_DEFAULT,
                      ?wxFONTSTYLE_NORMAL,
                      ?wxFONTWEIGHT_BOLD),

    Button = wxButton:new(Frame, ?wxID_BUTTON, [{label, "Перевірити"},
                                                {pos, {400, 5}},
                                                {size, {150, 50}}]),
    
    Result = wxStaticText:new(Frame, ?wxID_RESULT, "", [{pos, {550,0}},
                                                        {size,{400, 60}}]),
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

update_status(#state{frame = Frame, right = R, wrong = W, timer = Time}) ->
    Status = io_lib:format("Вірно: ~p      Невірно: ~p         Залишилось часу: ~p",
                           [R,W, Time]),
    wxFrame:setStatusText(Frame, Status, []).

disable_button(#state{frame = Frame}) ->
    Button = widgets:get_object(?wxID_BUTTON, Frame),
    wxButton:disable(Button).


