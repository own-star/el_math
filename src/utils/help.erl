%%% @author Taras J. Honcharuk
%%% @copyright (C) 2020, TheLostGameTeam
%%%  
%%% LICENSE GPLv3
%%%
%%% https://github.com/own-star/el_math
%%%

-module(help).

-export([get/1]).


-include_lib("wx/include/wx.hrl").
-include("el_math.hrl").


%get(?wxID_ABOUT) ->
%    {ok, Vsn} = application:get_key(?APP_NAME, vsn),
%    Text = io_lib:format("Арифметика для початкової школи~n~n"
%                         "Version: ~p~n~n"
%                         "Автор: Taras J. Honcharuk~n~n"
%                         "https://github.com/own-star/el_math~n~n"
%                         "License: GPLv3~n2020", [Vsn]),
%    wxMessageDialog:new(wx:null(), Text);


get(Cmd) ->
    Text = create_help(Cmd),

    wxMessageDialog:new(wx:null(), Text).


create_help(?wxID_ABOUT) ->

    {ok, Vsn} = application:get_key(?APP_NAME, vsn),
    io_lib:format("Арифметика для початкової школи~n~n"
                  "Version: ~p~n~n"
                  "Автор: Taras J. Honcharuk~n~n"
                  "https://github.com/own-star/el_math~n~n"
                  "License: GPLv3~n~n2020", [Vsn]);

create_help(Cmd) ->
    Cmd1 = Cmd rem 100,
    SignInt = Cmd1 div 10,
    Memeber = Cmd1 rem 10,

    create_help(SignInt, Memeber).

create_help(?HELP_MUL, 0) ->
    create_help(?HELP_MUL, 10);
create_help(?HELP_MUL, M) ->
    io_lib:format("Таблиця множення на ~p~n"
                  "~p x 1 = ~p~n"
                  "~p x 2 = ~p~n"
                  "~p x 3 = ~p~n"
                  "~p x 4 = ~p~n"
                  "~p x 5 = ~p~n"
                  "~p x 6 = ~p~n"
                  "~p x 7 = ~p~n"
                  "~p x 8 = ~p~n"
                  "~p x 9 = ~p~n"
                  "~p x 10 = ~p~n",
                  [M,
                   M,M,
                   M,M*2,
                   M,M*3,
                   M,M*4,
                   M,M*5,
                   M,M*6,
                   M,M*7,
                   M,M*8,
                   M,M*9,
                   M,M*10]
                 );

create_help(?HELP_DIV, 0) ->
    create_help(?HELP_DIV, 10);
create_help(?HELP_DIV, M) ->
    io_lib:format("Таблиця ділення на ~p~n"
                  "~p : ~p = 1~n"
                  "~p : ~p = 2~n"
                  "~p : ~p = 3~n"
                  "~p : ~p = 4~n"
                  "~p : ~p = 5~n"
                  "~p : ~p = 6~n"
                  "~p : ~p = 7~n"
                  "~p : ~p = 8~n"
                  "~p : ~p = 9~n"
                  "~p : ~p = 10~n",
                  [M,
                   M,M,
                   M*2,M,
                   M*3,M,
                   M*4,M,
                   M*5,M,
                   M*6,M,
                   M*7,M,
                   M*8,M,
                   M*9,M,
                   M*10,M]
                 );


create_help(_,_) ->
    "".
