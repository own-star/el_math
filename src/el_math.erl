%%% @author Taras J. Honcharuk
%%% @copyright (C) 2020, TheLostGameTeam
%%%  
%%% LICENSE GPLv3
%%%
%%% https://github.com/own-star/el_math
%%%

-module(el_math).

-behaviour(wx_object).

-export([start/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         handle_event/2, terminate/2, code_change/3]).

%-export([mul/2, mydiv/2, add/2, sub/2]).

-include_lib("wx/include/wx.hrl").

-include("el_math.hrl").

-define(SERVER, ?MODULE).

start() ->
    wx_object:start_link({local, ?SERVER}, ?MODULE, [], []).

init(_) ->
    Wx = wx:new(),
    Frame = wx:batch(fun() -> widgets:create_frame(Wx) end),
    wxWindow:show(Frame),

    {Frame, #state{frame = Frame}}.


   


handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({timeout, _, timer}, #state{timer_set = disable} = State) ->
    {noreply, State};
handle_info({timeout, _, timer}, #state{timer = 0} = State) ->
    widgets:disable_button(State),
    {noreply, State#state{timer = State#state.timer_set}};
handle_info({timeout, _, timer}, #state{timer = Time} = State) ->
    erlang:start_timer(1000, self(), timer),
    State1 = State#state{timer = Time - 1},
    widgets:update_status(State1),
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

handle_event(#wx{id = ?wxID_ABOUT}, State) ->
    wxMessageDialog:showModal(help:get(?wxID_ABOUT)),
    {noreply, State};

handle_event(#wx{id = Id}, State) when Id div 100 =:= 7 ->
    wxMessageDialog:showModal(help:get(Id)),
    {noreply, State};

handle_event(#wx{obj=Menu, id=?wxID_RANDOM}, State) ->
    case wxMenu:isChecked(Menu, ?wxID_RANDOM) of
        true ->
            {noreply, State#state{random = true}};
        _ ->
            {noreply, State#state{random = false}}
    end;

handle_event(#wx{obj=Frame, id=Id,
                 event=#wxCommand{type=command_menu_selected}}, State)->
    [R|Tail] = get_random_and_tail(State#state.random),
    {A, B, Action} = task:start(Frame, Id, R),
    State1 = State#state{a = A,
                         b = B,
                         action = Action,
                         command = Id,
                         right = 0,
                         wrong =0,
                         rand_list = Tail
                        },
    {noreply, State1};

handle_event(#wx{id=?wxID_BUTTON}, State) ->
    State1 = task:check(State),
    widgets:update_status(State1),
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




%%%%%%%%%%%%%%%%%%%%%%%%  INTERNAL %%%%%%%%%%%%%%%%%%%%%%%%%

get_random_and_tail(true) ->
    [0|[]];
get_random_and_tail(false) ->
    shuffle(lists:seq(1,9)).


shuffle(List) ->
    shuffle(List, []).

shuffle([], Acc) ->
    Acc;
shuffle(List, Acc) ->
    N = lists:nth(rand:uniform(length(List)), List),
    shuffle(lists:delete(N, List), [N|Acc]).


timer(600) ->
    disable;
timer(630) ->
    30;
timer(Command) ->
    Min = Command rem 10,
    Time = Min * 60,
    Time.

